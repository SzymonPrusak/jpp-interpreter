module TypeChecker where

import Control.Monad (guard)
import Control.Monad.Except (ExceptT, runExceptT, MonadError (throwError))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT), ask, local)
import qualified Data.Set as S
import qualified Data.Map as M

import Gram.Abs

import Debug.Trace (trace)



type FunMap = M.Map Ident FunDef
type VarMap = M.Map Ident TypeDef
type RetTypeInfo = Maybe TypeName
data TCEnv = TCEnv {
    funs :: FunMap,
    vars :: VarMap,
    retType :: RetTypeInfo
    }

emptyTcEnv :: TCEnv
emptyTcEnv = TCEnv M.empty M.empty Nothing


data Error =
    FunRedef Ident BNFC'Position
    | VarRedef Ident BNFC'Position
    | UndefVar Ident BNFC'Position
    | ReadOnlyAssign Ident BNFC'Position
    | TypeMissmatch Ident TypeName TypeName BNFC'Position
    | ExpectedArray Ident BNFC'Position
    | UnexpectedErr
    deriving (Show)

type TCReader a = ReaderT TCEnv (ExceptT Error Identity) a 


intTn :: TypeName
intTn = TNPrim Nothing $ PTInt Nothing
stringTn :: TypeName
stringTn = TNPrim Nothing $ PTString Nothing
boolTn :: TypeName
boolTn = TNPrim Nothing $ PTBool Nothing



runTc :: [FunDef] -> Either Error ()
runTc fs = (runIdentity . runExceptT . runReaderT (runTcReader fs)) emptyTcEnv where
    runTcReader :: [FunDef] -> TCReader ()
    runTcReader fs = processFuns fs $ tcFunDefs fs


processFuns :: [FunDef] -> TCReader () -> TCReader ()
processFuns fs fun = case checkFunRedefs fs of
    Right _ -> local (addFuns fs) fun
    Left err -> throwError err
    where
        checkFunRedefs :: [FunDef] -> Either Error (S.Set Ident)
        checkFunRedefs [] = return S.empty
        checkFunRedefs ((FunDefin pos _ name _ _):xs) = do
            s <- checkFunRedefs xs
            if S.member name s
                then throwError $ FunRedef name pos
                else return $ S.insert name s
        addFuns :: [FunDef] -> TCEnv -> TCEnv
        addFuns [] env = env
        addFuns (x:xs) env = env { funs = M.insert name fd $ funs $ addFuns xs env } where
            fd@(FunDefin _ _ name _ _) = x


tcFunDefs :: [FunDef] -> TCReader ()
tcFunDefs = foldr ((>>) . tcFunDef) (return ())
tcFunDef :: FunDef -> TCReader ()
tcFunDef (FunDefin pos retTn name params (StmtBlck _ bs)) = do
    trace (show name) $ pure ()
    processFuns sfs runSubTc
    where
        sfs = getSubFunDefs bs
        sss = getSubStmts bs
        runSubTc :: TCReader ()
        runSubTc = do
            tcFunDefs sfs
            local (addParams params . addRetType retTn) $ tcStmts sss
        
        getSubFunDefs :: [BlockStmt] -> [FunDef]
        getSubFunDefs ((BSFunDef _ fd):xs) = fd:getSubFunDefs xs
        getSubFunDefs (_:xs) = getSubFunDefs xs
        getSubFunDefs [] = []
        getSubStmts :: [BlockStmt] -> [Stmt]
        getSubStmts ((BSStmt _ s):xs) = s:getSubStmts xs
        getSubStmts (_:xs) = getSubStmts xs
        getSubStmts [] = []
        addParams :: [FunParam] -> TCEnv -> TCEnv
        addParams [] env = env
        addParams (x:xs) env = env { vars = M.insert name td $ vars $ addParams xs env } where
            fp@(FunPar _ td name) = x
        addRetType :: FunRet -> TCEnv -> TCEnv
        addRetType FRVoid {} env = env { retType = Nothing }
        addRetType (FRType _ tn) env = env { retType = Just tn }


tcStmts :: [Stmt] -> TCReader ()

tcStmts [] = return ()
tcStmts (SEmpty {}:xs) = tcStmts xs

tcStmts ((SDecl pos td name):xs) = do
    env <- ask
    if M.member name (vars env)
        then throwError $ VarRedef name pos
        else local (addVar td name) $ tcStmts xs

tcStmts ((SAssign pos (AVar _ name exp)):xs) = do
    env <- ask
    case checkAssign (getVarType name pos) env exp name pos of
        Right _ -> tcStmts xs
        Left err -> throwError err

tcStmts ((SArrAssign pos (AArrAcc _ a@(ArrAcc _ name posExp) exp)):xs) = do
    env <- ask
    case checkArrAssign env of
        Right _ -> tcStmts xs
        Left err -> throwError err
    where
        checkArrAssign env = do
            indEt <- tcExp posExp env
            if compareTypes indEt intTn
                then checkAssign (getArrAccType a) env exp name pos
                else throwError $ TypeMissmatch (Ident "index") intTn indEt pos

tcStmts (_:xs) = tcStmts xs

addVar :: TypeDef -> Ident -> TCEnv -> TCEnv
addVar td name env = env { vars = M.insert name td (vars env) }

getVarType :: Ident -> BNFC'Position -> TCEnv -> Either Error (TypeName, TypeMod)
getVarType name pos env = case M.lookup name (vars env) of
    Just (TypeDefin _ mod tn) -> return (tn, mod)
    Nothing -> throwError $ UndefVar name pos

getArrAccType :: ArrayAccess -> TCEnv -> Either Error (TypeName, TypeMod)
getArrAccType (ArrAcc pos name exp) env = do
    (at, mod) <- getVarType name pos env
    case at of
        (TNArr _ (TArrayType _ st)) -> return (st, mod)
        _ -> throwError $ ExpectedArray name pos

checkAssign :: (TCEnv -> Either Error (TypeName, TypeMod)) -> TCEnv -> Exp -> Ident -> BNFC'Position -> Either Error ()
checkAssign fun env exp name pos = do
    (tn, mod) <- fun env
    et <- tcExp exp env
    if not $ compareTypes tn et
        then throwError $ TypeMissmatch name tn et pos
        else case mod of
            TMNone {} -> return ()
            TMReadonly {} -> throwError $ ReadOnlyAssign name pos


compareTypes :: TypeName -> TypeName -> Bool
compareTypes (TNPrim _ t1) (TNPrim _ t2) = comparePrimTypes t1 t2 where
    comparePrimTypes :: PrimType -> PrimType -> Bool
    comparePrimTypes PTBool {} PTBool {} = True
    comparePrimTypes PTString {} PTString {} = True
    comparePrimTypes PTInt {} PTInt {} = True
    comparePrimTypes _ _ = False
compareTypes (TNArr _ (TArrayType _ t1)) (TNArr _ (TArrayType _ t2)) = compareTypes t1 t2
compareTypes (TNTuple _ (TTupleType _ t1)) (TNTuple _ (TTupleType _ t2)) = compareTupleSubTypes t1 t2 where
    compareTupleSubTypes :: [TupleSubType] -> [TupleSubType] -> Bool
    compareTupleSubTypes l1 l2
        | length l1 /= length l2 = False
        | otherwise = all doComparison $ zip t1 t2 where
            doComparison (TupleSType _ t1, TupleSType _ t2) = compareTypes t1 t2
compareTypes _ _ = False


tcExp :: Exp -> TCEnv -> Either Error TypeName
tcExp exp env = return intTn

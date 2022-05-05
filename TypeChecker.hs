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
    | UndefFun Ident BNFC'Position
    | InvalidFunArgs Ident [TypeName] [TypeName] BNFC'Position
    | VoidFunResult Ident BNFC'Position
    | UnsuppAdd TypeName TypeName BNFC'Position
    | UnexpectedErr
    deriving (Show)

type TCReader a = ReaderT TCEnv (ExceptT Error Identity) a 


intTn :: TypeName
intTn = TNPrim Nothing $ PTInt Nothing
stringTn :: TypeName
stringTn = TNPrim Nothing $ PTString Nothing
boolTn :: TypeName
boolTn = TNPrim Nothing $ PTBool Nothing

arrayTn :: TypeName -> TypeName
arrayTn tn = TNArr Nothing $ TArrayType Nothing tn

tupleTn :: [TypeName] -> TypeName
tupleTn ts = TNTuple Nothing $ TTupleType Nothing $ map (TupleSType Nothing) ts


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
            expectType intTn indEt (Ident "index") pos
            checkAssign (getArrAccType a) env exp name pos

tcStmts (_:xs) = tcStmts xs

addVar :: TypeDef -> Ident -> TCEnv -> TCEnv
addVar td name env = env { vars = M.insert name td (vars env) }

getVarType :: Ident -> BNFC'Position -> TCEnv -> Either Error (TypeName, TypeMod)
getVarType name pos env = case M.lookup name (vars env) of
    Just (TypeDefin _ mod tn) -> return (tn, mod)
    Nothing -> throwError $ UndefVar name pos

getArrAccType :: ArrayAccess -> TCEnv -> Either Error (TypeName, TypeMod)
getArrAccType (ArrAcc pos name _) env = do
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

tcExp EInt {} env = return intTn
tcExp EString {} env = return stringTn
tcExp EBool {} env = return boolTn

tcExp (EVarRef pos name) env = do
    (tn, _) <- getVarType name pos env
    return tn

tcExp (EArrInit pos (ArrInit _ tn cExp)) env = do
    cet <- tcExp cExp env
    expectType intTn cet (Ident "new[]") pos
    return $ arrayTn tn

tcExp (EArrConstr pos (ArrConstr _ els)) env = do
    etn <- tcEls els
    return $ arrayTn etn
    where
        tcEls :: [ConstrEl] -> Either Error TypeName
        tcEls [] = error "empty array construction - should be handled by grammar"
        tcEls [ConstrElem _ exp] = tcExp exp env
        tcEls ((ConstrElem elPos exp):xs) = do
            restTn <- tcEls xs
            expTn <- tcExp exp env
            expectType restTn expTn (Ident "array construction") elPos
            return expTn

tcExp (ETupleConstr pos (TupleConstr _ els)) env = do
    ts <- mapM (`tcConstrEl` env) els 
    return $ tupleTn ts

tcExp (EArrAcc pos a@(ArrAcc _ name indExp)) env = do
    (atn, _) <- getArrAccType a env
    indExpT <- tcExp indExp env
    expectType intTn indExpT (Ident "index") pos
    return atn

tcExp (EFunCall pos (FuncCall _ name args)) env = do
    (FunDefin _ rt _ params _) <- case M.lookup name (funs env) of
        Nothing -> throwError $ UndefFun name pos
        Just fd -> return fd
    rtn <- case rt of
        FRVoid {} -> throwError $ VoidFunResult name pos
        FRType _ tn -> return tn
    a <- argTypes args
    tcArgs (paramTypes params) a pos
    return rtn
    where
        paramTypes :: [FunParam] -> [TypeName]
        paramTypes p = map (\(FunPar _ (TypeDefin _ _ tn) _) -> tn) p
        argTypes :: [FunArg] -> Either Error [TypeName]
        argTypes a = mapM (\(FuncArg _ exp) -> tcExp exp env) a
        tcArgs :: [TypeName] -> [TypeName] -> BNFC'Position -> Either Error ()
        tcArgs params args pos
            | length params /= length args = funArgsErr
            | otherwise = if all (uncurry compareTypes) $ zip params args
                then return ()
                else funArgsErr
                where
                    funArgsErr = throwError $ InvalidFunArgs name params args pos

tcExp (EMul pos e1 _ e2) env = do
    expect2Int e1 e2 env pos
    return intTn

tcExp (EAdd pos e1 op e2) env = do
    t1 <- tcExp e1 env
    t2 <- tcExp e2 env
    expectType t1 t2 (Ident "addition") pos
    if isValidAdd t1 op
        then return t1
        else throwError $ UnsuppAdd t1 t2 pos
    where
        isValidAdd :: TypeName -> AddOp -> Bool
        isValidAdd tn AOPlus {} = compareTypes tn stringTn || compareTypes tn intTn
        isValidAdd tn AOMinus {} = compareTypes tn intTn

tcExp (EComp pos e1 op e2) env
    | isEqComp op = do
        t1 <- tcExp e1 env
        t2 <- tcExp e2 env
        expectType t1 t2 (Ident "equality") pos
        return boolTn
    | otherwise = do
        expect2Int e1 e2 env pos
        return boolTn
    where
        isEqComp :: CompOp -> Bool
        isEqComp COEq {} = True
        isEqComp CONeq {} = True
        isEqComp _ = False

tcExp (EAnd pos e1 e2) env = do
    expect2Bool e1 e2 env pos
    return boolTn

tcExp (EOr pos e1 e2) env = do
    expect2Bool e1 e2 env pos
    return boolTn

expectType :: TypeName -> TypeName -> Ident -> BNFC'Position -> Either Error ()
expectType expected actual name pos = if compareTypes expected actual
    then return ()
    else throwError $ TypeMissmatch name expected actual pos

expect :: TypeName -> Exp -> TCEnv -> Ident -> BNFC'Position -> Either Error ()
expect expected exp env name pos = do
    tn <- tcExp exp env
    expectType expected tn name pos

expect2 :: TypeName -> Exp -> Exp -> TCEnv -> BNFC'Position -> Either Error ()
expect2 expected e1 e2 env pos = do
    expect expected e1 env (Ident "left") pos
    expect expected e2 env (Ident "right") pos

expect2Int :: Exp -> Exp -> TCEnv -> BNFC'Position -> Either Error ()
expect2Int = expect2 intTn

expect2Bool :: Exp -> Exp -> TCEnv -> BNFC'Position -> Either Error ()
expect2Bool = expect2 boolTn

tcConstrEl :: ConstrEl -> TCEnv -> Either Error TypeName
tcConstrEl (ConstrElem _ exp) = tcExp exp

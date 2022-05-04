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
    | UnexpectedErr
    deriving (Show)

type TCReader a = ReaderT TCEnv (ExceptT Error Identity) a 


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
    tcStmts xs
tcStmts ((SAssign pos avar):xs) = do
    env <- ask
    case checkAssign avar (vars env) of
        Right _ -> tcStmts xs
        Left err -> throwError err

tcStmts _ = return ()

checkAssign :: VarAssign -> VarMap -> Either Error ()
checkAssign (AVar pos name exp) vars = do
    (TypeDefin _ mod _) <- case M.lookup name vars of
        Just td -> return td
        Nothing -> throwError $ UndefVar name pos
    case mod of
        TMNone {} -> return ()
        TMReadonly {} -> throwError $ ReadOnlyAssign name pos

addVar :: TypeDef -> Ident -> TCEnv -> TCEnv
addVar td name env = env { vars = M.insert name td (vars env) }

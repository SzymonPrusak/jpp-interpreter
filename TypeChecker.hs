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
type TCEnv = (FunMap, VarMap)

data Error = FunctionRedefinition Ident BNFC'Position
    deriving (Show)

type TCReader a = ReaderT TCEnv (ExceptT Error Identity) a 



runTc :: [FunDef] -> Either Error ()
runTc fs = (runIdentity . runExceptT . runReaderT (runTcReader fs)) (M.empty, M.empty) where
    runTcReader :: [FunDef] -> TCReader ()
    runTcReader fs = do
        processFuns fs


processFuns :: [FunDef] -> TCReader ()
processFuns fs = case checkRedefs fs of
    Right _ -> local (addFuns fs) $ tcFunDefs fs
    Left err -> throwError err
    where
        checkRedefs :: [FunDef] -> Either Error (S.Set Ident)
        checkRedefs [] = return S.empty
        checkRedefs ((FunDefin pos _ name _ _):xs) = do
            s <- checkRedefs xs
            if S.member name s
                then throwError $ FunctionRedefinition name pos
                else return $ S.insert name s
        addFuns :: [FunDef] -> TCEnv -> TCEnv
        addFuns [] env = env
        addFuns (x:xs) env@(fm, vm) = (M.insert name fd $ fst $ addFuns xs env, vm) where
            fd@(FunDefin _ _ name _ _) = x


tcFunDefs :: [FunDef] -> TCReader ()
tcFunDefs [] = return ()
tcFunDefs (x:xs) = do
    tcFunDef x
    tcFunDefs xs
    where
        tcFunDef :: FunDef -> TCReader ()
        tcFunDef (FunDefin pos tn name params body) = do
            trace (show name) $ pure ()
            tcStmtBlock body


tcStmtBlock :: StmtBlock -> TCReader ()
tcStmtBlock (StmtBlck _ bs) = processFuns $ getSubFunDefs bs where
    getSubFunDefs :: [BlockStmt] -> [FunDef]
    getSubFunDefs ((BSFunDef _ fd):xs) = fd:getSubFunDefs xs
    getSubFunDefs (_:xs) = getSubFunDefs xs
    getSubFunDefs [] = []

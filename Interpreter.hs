module Interpreter where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask, local))
import Control.Monad.State (StateT, MonadState (get, put), evalStateT)

import Common
import Gram.Abs
import Gram.Print (printTree)



type FunName = Ident
type BlockQName = [Ident]

type FunMap = M.Map FunName FunDef
type CallMap = M.Map BlockQName FunMap


buildCallMap :: [FunDef] -> CallMap
buildCallMap fds = processLocalFuns fds [] M.empty where
    processLocalFuns :: [FunDef] -> BlockQName -> CallMap -> CallMap
    processLocalFuns fds ns m = foldr (`buildCallMapF` ns) (M.insert ns (createFunMap fds) m) fds where
        createFunMap :: [FunDef] -> FunMap
        createFunMap = foldr (\fd@(FunDefin _ _ name _ _) -> M.insert name fd) M.empty

    buildCallMapF :: FunDef -> BlockQName -> CallMap -> CallMap
    buildCallMapF (FunDefin _ _ name _ sb) ns m = buildCallMapB sb (name:ns) m
    buildCallMapB :: StmtBlock -> BlockQName -> CallMap -> CallMap
    buildCallMapB (StmtBlck _ bs) ns m = buildCallMapS 0 ss ns lfCallMap where
        sfs = getSubFunDefs bs
        ss = getSubStmts bs
        lfCallMap = if null sfs
            then m
            else processLocalFuns sfs ns m

        buildCallMapS :: Int -> [Stmt] -> BlockQName -> CallMap -> CallMap
        buildCallMapS _ [] _ m = m
        buildCallMapS bi (s:ss) ns m = case s of
            SSubBlock _ sb -> addSingleBlock sb
            SIf _ (IfBr _ _ (SSubBlock _ sb)) -> addSingleBlock sb
            SIfEl _ ifel -> case ifel of
                IfElBr _ _ tsb (SSubBlock _ fsb) -> buildCallMapB fsb (getSubBlockName 1) $ buildCallMapB tsb (getSubBlockName 0) $ getRestCallMap 2
                IfElBr _ _ tsb _ -> addSingleBlock tsb
            SLoopWhile _ (LWhile _ _ (SSubBlock _ sb)) -> addSingleBlock sb
            SLoopFor _ (LFor _ _ _ (SSubBlock _ sb)) -> addSingleBlock sb
            _ -> getRestCallMap 0
            where
                getRestCallMap :: Int -> CallMap
                getRestCallMap i = buildCallMapS (bi + i) ss ns m
                getSubBlockName :: Int -> BlockQName
                getSubBlockName i = Ident (show (bi + i)):ns
                addSingleBlock :: StmtBlock -> CallMap
                addSingleBlock sb = buildCallMapB sb (getSubBlockName 0) $ getRestCallMap 1


type VarAddress = Int
data VarValue =
    VInt Int
    | VString String
    | VBool Bool
    | VArray [VarValue]
    deriving (Eq)
type VarStack = M.Map VarAddress VarValue

data IPGlobalEnv = IPGEnv {
    callMap :: CallMap,
    stack :: VarStack,
    nextAddress :: Int
    }

newGlobalEnv :: [FunDef] -> IPGlobalEnv
newGlobalEnv fds = IPGEnv { callMap = buildCallMap fds, stack = M.empty, nextAddress = 0 }


type VarName = Ident
type VarMap = M.Map VarName VarAddress

data IPLocalEnv = IPLEnv {
    blockName :: BlockQName,
    varMap :: VarMap,
    nextSubBlockIndex :: Int
    }

type IPLEnvMod = IPLocalEnv -> IPLocalEnv

newLocalEnv :: IPLocalEnv
newLocalEnv = IPLEnv { blockName = [], varMap = M.empty, nextSubBlockIndex = 0 }

setBlock :: BlockQName -> IPLEnvMod
setBlock ns env = env { blockName = ns, nextSubBlockIndex = 0 }

enterBlock :: Ident -> IPLEnvMod
enterBlock n env = setBlock (n:blockName env) env

bindVar :: VarName -> VarAddress -> IPLEnvMod
bindVar vn addr env = env { varMap = M.insert vn addr $ varMap env }

nextSubBlock :: IPLEnvMod
nextSubBlock env = env { nextSubBlockIndex = nextSubBlockIndex env + 1 }


data RuntimeException =
    EntryPointNotFound
    deriving (Show)

type IPResult a = Either RuntimeException a
type Interpreter a = StateT IPGlobalEnv (ReaderT IPLocalEnv (ExceptT RuntimeException Identity)) a


fatalError m = error $ "interpreter fatal: " ++ m


runInterpreter :: [FunDef] -> IPResult ()
runInterpreter fds = (runIdentity . runExceptT . (`runReaderT` newLocalEnv) . (`evalStateT` newGlobalEnv fds)) runInterpreter  where
    runInterpreter :: Interpreter ()
    runInterpreter = do
        mmain <- resolveCall (Ident "main")
        (main, ns) <- maybe (throwError EntryPointNotFound) return mmain
        callFunDef main ns []
        return ()


resolveCall :: FunName -> Interpreter (Maybe (FunDef, BlockQName))
resolveCall fn = do
    genv <- get
    lenv <- ask
    return $ resolveCall fn (blockName lenv) (callMap genv)
    where
        resolveCall :: FunName -> BlockQName -> CallMap -> Maybe (FunDef, BlockQName)
        resolveCall fn [] m = do
            fm <- M.lookup [] m
            lookupFun fn fm []
        resolveCall fn ns@(i:is) m = case M.lookup ns m of
            Nothing -> fallback
            Just fm -> maybe fallback return $ lookupFun fn fm (i:is)
            where
                fallback = resolveCall fn is m
        lookupFun :: FunName -> FunMap -> BlockQName -> Maybe (FunDef, BlockQName)
        lookupFun fn fm ns = (\d -> (d, ns)) <$> M.lookup fn fm

callFunction :: FunName -> [VarValue] -> Interpreter (Maybe VarValue)
callFunction fn args = do
    mfun <- resolveCall fn
    case mfun of
        Nothing -> fatalError $ "function " ++ printTree fn ++ " not found - should be handled by TC"
        Just (fd, ns) -> callFunDef fd ns args

callFunDef :: FunDef -> BlockQName -> [VarValue] -> Interpreter (Maybe VarValue)
callFunDef (FunDefin _ _ fn params sb) ns args = do
    lenv1 <- setArguments params args
    let lenv2 = setBlock (fn:ns) lenv2
    local (const lenv2) $ execBlock sb
    where
        setArguments :: [FunParam] -> [VarValue] -> Interpreter IPLocalEnv
        setArguments ((FunPar _ _ vn):ps) (a:as) = do
            addr <- allocVar
            setVar addr a
            local (bindVar vn addr) $ setArguments ps as
        setArguments _ _ = ask


allocVar :: Interpreter VarAddress
allocVar = do
    genv <- get
    let addr = nextAddress genv
    put genv { nextAddress = addr + 1 }
    return addr

setVar :: VarAddress -> VarValue -> Interpreter ()
setVar addr val = do
    genv <- get
    let nstack = M.insert addr val $ stack genv
    put genv { stack = nstack }


execBlock :: StmtBlock -> Interpreter (Maybe VarValue)
execBlock = undefined

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



fatalError m = error $ "interpreter fatal: " ++ m


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


data VarValue =
    VInt Int
    | VString String
    | VBool Bool
    | VArray [VarValue]
    | VTuple [VarValue]
    deriving (Eq)

defaultValue :: TypeName -> VarValue
defaultValue (TNPrim _ (PTBool _)) = VBool False
defaultValue (TNPrim _ (PTInt _)) = VInt 0
defaultValue (TNPrim _ (PTString _)) = VString ""
defaultValue (TNArr _ _) = VArray []
defaultValue (TNTuple _ (TTupleType _ sts)) = VArray $ map (\(TupleSType _ tn) -> defaultValue tn) sts

sureInt :: VarValue -> Int
sureInt (VInt i) = i
sureInt _ = fatalError "expected int value - should be handled by TC"

sureString :: VarValue -> String
sureString (VString i) = i
sureString _ = fatalError "expected string value - should be handled by TC"

sureBool :: VarValue -> Bool
sureBool (VBool b) = b
sureBool _ = fatalError "expected bool value - should be handled by TC"

sureTuplArr :: VarValue -> [VarValue]
sureTuplArr (VArray l) = l
sureTuplArr (VTuple l) = l
sureTuplArr _ = fatalError "expected array/tuple value - should be handled by TC"


type VarAddress = Int
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
    | VarNotInitialized VarName BlockQName
    deriving (Show)

type IPResult a = Either RuntimeException a
type Interpreter a = StateT IPGlobalEnv (ReaderT IPLocalEnv (ExceptT RuntimeException Identity)) a


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
callFunDef (FunDefin _ fr fn params sb) ns args = do
    lenv1 <- setArguments params args
    let lenv2 = setBlock (fn:ns) lenv2
    rv <- local (const lenv2) $ execBlock sb
    case fr of
        FRVoid {} -> return Nothing
        FRType _ tn -> return $ Just $ fromMaybe (defaultValue tn) rv
    where
        setArguments :: [FunParam] -> [VarValue] -> Interpreter IPLocalEnv
        setArguments ((FunPar _ _ vn):ps) (a:as) = do
            addr <- allocVar
            setVar addr a
            local (bindVar vn addr) $ setArguments ps as
        setArguments _ _ = ask


sure :: Maybe a -> String -> a
sure (Just x) m = x
sure Nothing m = fatalError m


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

readVar :: VarName -> Interpreter VarValue
readVar name = do
    genv <- get
    lenv <- ask
    let addr = sure (M.lookup name $ varMap lenv) "undefined variable - should be handled by TC"
    maybe (throwError $ VarNotInitialized name $ blockName lenv) return $ M.lookup addr $ stack genv


execBlock :: StmtBlock -> Interpreter (Maybe VarValue)
execBlock (StmtBlck _ bs) = execBlock $ getSubStmts bs where
    execBlock :: [Stmt] -> Interpreter (Maybe VarValue)
    execBlock [] = return Nothing
    execBlock (s:ss) = do
        r <- execStmt s
        case r of
            Left v -> return $ Just v
            Right m -> local m $ execBlock ss


type StmtResult = Either VarValue IPLEnvMod

execStmt :: Stmt -> Interpreter StmtResult

execStmt (SEmpty _) = return $ Right id

execStmt (SDecl _ _ name) = Right . bindVar name <$> allocVar


evalExp :: Exp -> Interpreter VarValue

evalExp (EInt _ l) = case l of
    (LInt _ i) -> return $ VInt $ fromIntegral i
    (LNInt _ i) -> return $ VInt $ fromIntegral (-i)

evalExp (EString _ s) = return $ VString s

evalExp (EBool _ b) = case b of
    (BTrue _) -> return $ VBool True
    (BFalse _) -> return $ VBool False

evalExp (EVarRef _ name) = readVar name

evalExp (EArrInit _ (ArrInit _ tn cExp)) = do
    cInt <- evalInt cExp
    return $ VArray $ [defaultEl | i <- [1..cInt]]
    where
        defaultEl = defaultValue tn

evalExp (EArrConstr _ (ArrConstr _ els)) = evalConstr els VArray
evalExp (ETupleConstr _ (TupleConstr _ els)) = evalConstr els VTuple

evalExp (EArrAcc _ (ArrAcc _ name iExp)) = do
    var <- readVar name
    let els = sureTuplArr var
    iInt <- evalInt iExp
    return $ els !! iInt

evalExp (EFunCall _ (FuncCall _ name args)) = do
    eArgs <- mapM (\(FuncArg _ exp) -> evalExp exp) args
    resM <- callFunction name eArgs
    let res = sure resM "function did not return any value - should be handled by TC"
    return res

evalExp (EMul _ e1 op e2) = do
    v1 <- evalInt e1
    v2 <- evalInt e2
    return $ VInt $ getOp op v1 v2
    where
        getOp :: MulOp -> (Int -> Int -> Int)
        getOp MOMul {} = (*)
        getOp MODiv {} = div
        getOp MOMod {} = mod

evalExp (EAdd _ e1 op e2) = do
    v1 <- evalExp e1
    case v1 of
        (VInt v1i) -> do
            v2i <- evalInt e2
            return $ VInt $ getIntOp op v1i v2i
        (VString v1s) -> do
            case op of
                AOMinus {} -> wrongExp
                AOPlus {} -> do
                    v2s <- evalString e2
                    return $ VString $ v1s ++ v2s
        _ -> wrongExp
    where
        wrongExp = fatalError "invalid addition expression - should be handled by TC"
        getIntOp :: AddOp -> (Int -> Int -> Int)
        getIntOp AOPlus {} = (+)
        getIntOp AOMinus {} = (-)

evalExp (EComp _ e1 op e2) = do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case op of
        COEq {} -> return $ VBool $ v1 == v2
        CONeq {} -> return $ VBool $ v1 /= v2
        _ -> do
            let v1i = sureInt v1
            let v2i = sureInt v2
            return $ VBool $ getIntOp op v1i v2i
            where
                getIntOp :: CompOp -> (Int -> Int -> Bool)
                getIntOp COGt {} = (>)
                getIntOp COGe {} = (>=)
                getIntOp COLt {} = (<)
                getIntOp COLe {} = (<=)
                getIntOp _ = undefined

evalExp (EAnd _ e1 e2) = evalBoolOp e1 e2 (&&)
evalExp (EOr _ e1 e2) = evalBoolOp e1 e2 (||)


evalConstr :: [ConstrEl] -> ([VarValue] -> VarValue) -> Interpreter VarValue
evalConstr els ctor = do
    m <- mapM (\(ConstrElem _ elExp) -> evalExp elExp) els
    return $ ctor m

evalBoolOp :: Exp -> Exp -> (Bool -> Bool -> Bool) -> Interpreter VarValue
evalBoolOp e1 e2 op = do
    v1 <- evalBool e1
    v2 <- evalBool e2
    return $ VBool $ op v1 v2

evalInt :: Exp -> Interpreter Int
evalInt e = do
    eVal <- evalExp e
    return $ sureInt eVal

evalString :: Exp -> Interpreter String
evalString e = do
    eVal <- evalExp e
    return $ sureString eVal

evalBool :: Exp -> Interpreter Bool
evalBool e = do
    eVal <- evalExp e
    return $ sureBool eVal

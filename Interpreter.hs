module Interpreter where

import Data.Char (toLower)
import Data.Foldable (Foldable(toList))
import qualified Data.Map as M
import qualified Data.Sequence as S
import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, MonadIO (liftIO))
import Control.Monad.Identity (IdentityT (runIdentityT))
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask, local))
import Control.Monad.State (StateT, MonadState (get, put), evalStateT)

import Common
import Gram.Abs
import Gram.Print (printTree)



-- ================================================
import Data.List (intercalate)


printNs :: BlockQName -> String
printNs ns = intercalate ":" $ map printTree ns

printCallMap :: CallMap -> String
printCallMap cm = intercalate ",\n" els where
    els = map (\(ns, funMap) -> "(" ++ printNs ns ++ ", { " ++ printFunMap funMap ++ " })") $ M.toList cm

printFunMap :: FunMap -> String
printFunMap fm = intercalate ",\n    " $ map ((++ "()") . printTree . fst) $ M.toList fm
-- ================================================



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
            SIf _ (IfBr _ _ s) -> addStmt s
            SIfEl _ ifel -> case ifel of
                IfElBr _ _ tsb (SSubBlock _ fsb) -> buildCallMapB fsb (getSubBlockName 1) $ buildCallMapB tsb (getSubBlockName 0) $ getRestCallMap 2
                IfElBr _ _ tsb _ -> buildCallMapB tsb (getSubBlockName 0) $ getRestCallMap 2
            SLoopWhile _ (LWhile _ _ s) -> addStmt s
            SLoopFor _ (LFor _ _ _ s) -> addStmt s
            _ -> getRestCallMap 0
            where
                getRestCallMap :: Int -> CallMap
                getRestCallMap i = buildCallMapS (bi + i) ss ns m
                getSubBlockName :: Int -> BlockQName
                getSubBlockName i = Ident (show (bi + i)):ns
                addSingleBlock :: StmtBlock -> CallMap
                addSingleBlock sb = buildCallMapB sb (getSubBlockName 0) $ getRestCallMap 1
                addStmt :: Stmt -> CallMap
                addStmt (SSubBlock _ sb) = addSingleBlock sb
                addStmt _ = getRestCallMap 1


data VarValue =
    VInt Int
    | VString String
    | VBool Bool
    | VArray (S.Seq VarValue)
    | VTuple (S.Seq VarValue)
    deriving (Eq)

defaultValue :: TypeName -> VarValue
defaultValue (TNPrim _ (PTBool _)) = VBool False
defaultValue (TNPrim _ (PTInt _)) = VInt 0
defaultValue (TNPrim _ (PTString _)) = VString ""
defaultValue (TNArr _ _) = VArray S.empty
defaultValue (TNTuple _ (TTupleType _ sts)) = VArray $ S.fromList $ map (\(TupleSType _ tn) -> defaultValue tn) sts

sureInt :: VarValue -> Int
sureInt (VInt i) = i
sureInt _ = fatalError "expected int value - should be handled by TC"

sureString :: VarValue -> String
sureString (VString i) = i
sureString _ = fatalError "expected string value - should be handled by TC"

sureBool :: VarValue -> Bool
sureBool (VBool b) = b
sureBool _ = fatalError "expected bool value - should be handled by TC"

sureTuplArr :: VarValue -> S.Seq VarValue
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

enterNextBlock :: IPLEnvMod
enterNextBlock env = enterBlock (Ident $ show $ nextSubBlockIndex env) env

bindVar :: VarName -> VarAddress -> IPLEnvMod
bindVar vn addr env = env { varMap = M.insert vn addr $ varMap env }

clearVars :: IPLEnvMod
clearVars env = env { varMap = M.empty }

incNextSubBlock :: IPLEnvMod
incNextSubBlock env = env { nextSubBlockIndex = nextSubBlockIndex env + 1 }


data RuntimeException =
    ExcEntryPointNotFound
    | ExcVarNotInitialized VarName BNFC'Position
    | ExcDivideByZero BNFC'Position
    | ExcIndexOutOfBounds VarName BNFC'Position
    | ExcNegativeArrSize BNFC'Position
    deriving (Show)

type IPResult a = Either RuntimeException a
type Interpreter a = StateT IPGlobalEnv (ReaderT IPLocalEnv (ExceptT RuntimeException (IdentityT IO))) a


runInterpreter :: [FunDef] -> IO (IPResult ())
runInterpreter fds = (runIdentityT . runExceptT . (`runReaderT` newLocalEnv) . (`evalStateT` newGlobalEnv fds)) runInterpreter  where
    runInterpreter :: Interpreter ()
    runInterpreter = do
        genv <- get
        liftIO $ putStrLn $ printCallMap $ callMap genv
        mmain <- resolveCall (Ident "main")
        (main@(FunDefin _ _ _ args _), ns) <- maybe (throwError ExcEntryPointNotFound) return mmain
        let defArgs = map (\(FunPar _ (TypeDefin _ tn _) _) -> defaultValue tn) args
        callFunDef main ns defArgs
        return ()


funPrintS :: [VarValue] -> Bool -> Interpreter (Maybe VarValue)
funPrintS [VString s] = funPrint s
funPrintS _ = fatalError "invalid printS call - should be handled by TC"

funPrintI :: [VarValue] -> Bool -> Interpreter (Maybe VarValue)
funPrintI [VInt i] = funPrint $ show i
funPrintI _ = fatalError "invalid printI call - should be handled by TC"

funPrintB :: [VarValue] -> Bool -> Interpreter (Maybe VarValue)
funPrintB [VBool b] = funPrint $ map toLower $ show b
funPrintB _ = fatalError "invalid printS call - should be handled by TC"

funPrint :: String -> Bool -> Interpreter (Maybe VarValue)
funPrint s endl = liftIO $ printFun s >> return Nothing where
    printFun = if endl then putStrLn else putStr


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
callFunction fn@(Ident s) args = do
    mfun <- resolveCall fn
    case mfun of
        Nothing -> case s of
            "printS" -> funPrintS args False
            "printLnS" -> funPrintS args True
            "printI" -> funPrintI args False
            "printLnI" -> funPrintI args True
            "printB" -> funPrintB args False
            "printLnB" -> funPrintB args True
            _ -> fatalError $ "function " ++ printTree fn ++ " not found - should be handled by TC"
        Just (fd, ns) -> callFunDef fd ns args

callFunDef :: FunDef -> BlockQName -> [VarValue] -> Interpreter (Maybe VarValue)
callFunDef (FunDefin _ fr fn params sb) ns args = do
    local clearVars $ do
        lenv1 <- setArguments params args
        let lenv2 = setBlock (fn:ns) lenv1
        rv <- local (const lenv2) $ execBlock sb
        case fr of
            FRVoid {} -> return Nothing
            FRType _ tn -> case rv of
                Nothing -> return $ Just $ defaultValue tn
                Just (BResValue v) -> return $ Just v
                _ -> fatalError "invalid block evaluation result for function call - should be handled by TC"
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

getVarAddr :: VarName -> Interpreter VarAddress
getVarAddr name = do
    lenv <- ask
    let addr = sure (M.lookup name $ varMap lenv) "undefined variable - should be handled by TC"
    return addr

setVar :: VarAddress -> VarValue -> Interpreter ()
setVar addr val = do
    genv <- get
    let nstack = M.insert addr val $ stack genv
    put genv { stack = nstack }

readVar :: VarName -> BNFC'Position -> Interpreter VarValue
readVar name pos = do
    addr <- getVarAddr name
    readVarA addr name pos

readVarA :: VarAddress -> VarName -> BNFC'Position -> Interpreter VarValue
readVarA addr name pos = do
    genv <- get
    maybe (throwError $ ExcVarNotInitialized name pos) return $ M.lookup addr $ stack genv


data BlockResult =
    BResVoid
    | BResValue VarValue
    | BResBreak
    | BResContinue

execBlock :: StmtBlock -> Interpreter (Maybe BlockResult)
execBlock (StmtBlck _ bs) = execBlock $ getSubStmts bs where
    execBlock :: [Stmt] -> Interpreter (Maybe BlockResult)
    execBlock [] = return Nothing
    execBlock (s:ss) = do
        r <- execStmt s
        case r of
            SResSkip br -> return $ Just br
            SResCont m -> local m $ execBlock ss


data StmtResult =
    SResSkip BlockResult
    | SResCont IPLEnvMod

execStmt :: Stmt -> Interpreter StmtResult

execStmt (SEmpty _) = return $ SResCont id

execStmt (SDecl _ _ name) = SResCont . bindVar name <$> allocVar

execStmt (SAssign _ (AVar _ name exp)) = do
    addr <- getVarAddr name
    val <- evalExp exp
    setVar addr val
    return $ SResCont id

execStmt (SArrAssign pos (AArrAcc _ (ArrAcc _ name iExp) vExp)) = do
    addr <- getVarAddr name
    vVal <- evalExp vExp
    iVal <- evalInt iExp
    arr <- readVarA addr name pos
    case arr of
        VArray vals -> do
            when (iVal < 0 || S.length vals <= iVal) $ throwError $ ExcIndexOutOfBounds name pos
            let nArr = S.update iVal vVal vals
            setVar addr $ VArray nArr
            return $ SResCont id
        _ -> fatalError "expected array - should be handled by TC"

execStmt (SDeclAssign _ d vExp) = do
    vVal <- evalExp vExp
    m <- execDeclAssign d vVal
    return $ SResCont m

execStmt (SFunCall _ fc) = do
    evalFunCall fc
    return $ SResCont id

execStmt (SIf _ (IfBr _ cExp ts)) = do
    cVal <- evalBool cExp
    let contRes = SResCont incNextSubBlock
    if cVal
        then do
            sRes <- execStmt ts
            case sRes of
                ss@SResSkip {} -> return ss
                SResCont {} -> return contRes
        else return contRes
    

execStmt (SIfEl _ (IfElBr _ cExp tsb fs)) = do
    cVal <- evalBool cExp
    let contRes = SResCont (incNextSubBlock . incNextSubBlock)
    if cVal
        then do
            rb <- local enterNextBlock $ execBlock tsb
            case rb of
                Nothing -> return contRes
                Just b -> return $ SResSkip b
        else do
            rb <- local incNextSubBlock $ execStmt fs
            case rb of
                ss@SResSkip {} -> return ss
                SResCont {} -> return contRes

execStmt lw@(SLoopWhile _ (LWhile _ cExp s)) = execWhileLoop cExp s

execStmt (SLoopFor _ (LFor _ (AVar _ name svExp) evExp s)) = do
    svVal <- evalInt svExp
    evVal <- evalInt evExp
    itAddr <- allocVar
    local (bindVar name itAddr) $ execForLoop itAddr svVal evVal s

execStmt (SReturn _) = return $ SResSkip BResVoid

execStmt (SReturnVal _ exp) = do
    val <- evalExp exp
    return $ SResSkip $ BResValue val

execStmt (SContinue _) = return $ SResSkip BResContinue

execStmt (SBreak _) = return $ SResSkip BResBreak

execStmt (SSubBlock _ sb) = do
    rb <- local enterNextBlock $ execBlock sb
    case rb of
        Nothing -> return $ SResCont incNextSubBlock
        Just b -> return $ SResSkip b


execDeclAssign :: DeclA -> VarValue -> Interpreter IPLEnvMod
execDeclAssign (DeclASingl _ _ name) value = do
    addr <- allocVar
    setVar addr value
    return $ bindVar name addr
execDeclAssign (DeclATuple _ ds) (VTuple vs) = do
    let dvs = zip ds $ toList vs
    m <- mapM (uncurry execDeclAssign) dvs
    return $ foldr (.) id m
execDeclAssign _ _ = fatalError "invalid decl assign - should be handled by TC"


execWhileLoop :: Exp -> Stmt -> Interpreter StmtResult
execWhileLoop cExp s = do
    cVal <- evalBool cExp
    let contRes = SResCont incNextSubBlock
        nextIt = execWhileLoop cExp s
    if cVal
        then execLoopBody s contRes nextIt
        else return contRes

execForLoop :: VarAddress -> Int -> Int -> Stmt -> Interpreter StmtResult
execForLoop itAddr curIt maxIt s = do
    let contRes = SResCont incNextSubBlock
        nextIt = execForLoop itAddr (curIt + 1) maxIt s
    if curIt > maxIt 
        then return contRes
        else do
            setVar itAddr $ VInt curIt
            execLoopBody s contRes nextIt

execLoopBody :: Stmt -> StmtResult -> Interpreter StmtResult -> Interpreter StmtResult
execLoopBody body contRes nextIt = do
    sRes <- execStmt body
    case sRes of
        SResCont {} -> nextIt
        ss@(SResSkip bRes) -> case bRes of
            BResContinue {} -> nextIt
            BResBreak {} -> return contRes
            _ -> return ss


evalExp :: Exp -> Interpreter VarValue

evalExp (EInt _ l) = case l of
    (LInt _ i) -> return $ VInt $ fromIntegral i
    (LNInt _ i) -> return $ VInt $ fromIntegral (-i)

evalExp (EString _ s) = return $ VString s

evalExp (EBool _ b) = case b of
    (BTrue _) -> return $ VBool True
    (BFalse _) -> return $ VBool False

evalExp (EVarRef pos name) = readVar name pos

evalExp (EArrInit pos (ArrInit _ tn cExp)) = do
    cInt <- evalInt cExp
    when (cInt < 0) $ throwError $ ExcNegativeArrSize pos
    return $ VArray $ S.replicate cInt $ defaultValue tn

evalExp (EArrConstr _ (ArrConstr _ els)) = evalConstr els VArray
evalExp (ETupleConstr _ (TupleConstr _ els)) = evalConstr els VTuple

evalExp (EArrAcc pos (ArrAcc _ name iExp)) = do
    var <- readVar name pos
    let els = sureTuplArr var
    iInt <- evalInt iExp
    when (iInt < 0 || S.length els <= iInt) $ throwError $ ExcIndexOutOfBounds name pos
    return $ S.index els iInt

evalExp (EFunCall _ fc) = do
    resM <- evalFunCall fc
    let res = sure resM "function did not return any value - should be handled by TC"
    return res

evalExp (EMul pos e1 op e2) = do
    v1 <- evalInt e1
    v2 <- evalInt e2
    case calc v1 v2 op of
        Nothing -> do
            lenv <- ask
            throwError $ ExcDivideByZero pos
        Just v -> return $ VInt v
    where
        calc :: Int -> Int -> MulOp -> Maybe Int
        calc v1 v2 MOMul {} = Just $ v1 * v2
        calc v1 v2 MODiv {} = if v2 == 0 then Nothing else Just $ v1 `div` v2
        calc v1 v2 MOMod {} = if v2 == 0 then Nothing else Just $ v1 `mod` v2

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


evalConstr :: [ConstrEl] -> (S.Seq VarValue -> VarValue) -> Interpreter VarValue
evalConstr els ctor = do
    m <- mapM (\(ConstrElem _ elExp) -> evalExp elExp) els
    return $ ctor $ S.fromList m

evalFunCall :: FunCall -> Interpreter (Maybe VarValue)
evalFunCall (FuncCall _ name args) = do
    eArgs <- mapM (\(FuncArg _ exp) -> evalExp exp) args
    callFunction name eArgs

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

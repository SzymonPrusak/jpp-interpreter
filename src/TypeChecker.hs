module TypeChecker where

import Control.Monad (liftM2, when, unless)
import Control.Monad.Except (ExceptT, runExceptT, MonadError (throwError), liftEither)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT), ask, local)
import qualified Data.Set as S
import qualified Data.Map as M

import Gram.Abs
import Common
import TypeHelper



type FunMap = M.Map Ident FunDef
type VarMap = M.Map Ident TypeDef
type RetTypeInfo = Maybe TypeName
data TCEnv = TCEnv {
    funs :: FunMap,
    vars :: VarMap,
    retType :: RetTypeInfo,
    inLoop :: Bool
    }

builtInFd :: String -> [(TypeName, String)] -> FunDef
builtInFd name params = FunDefin Nothing (FRVoid Nothing) (Ident name) paramDefs (StmtBlck Nothing []) where
    paramDefs = map (\(tn, pName) -> FunPar Nothing (readWriteTd tn) (Ident pName)) params
printSFd :: FunDef
printSFd = builtInFd "printS" [(stringTn, "s")]
printBFd :: FunDef
printBFd = builtInFd "printB" [(boolTn, "b")]
printIFd :: FunDef
printIFd = builtInFd "printI" [(intTn, "i")]
printSLnFd :: FunDef
printSLnFd = builtInFd "printLnS" [(stringTn, "s")]
printBLnFd :: FunDef
printBLnFd = builtInFd "printLnB" [(boolTn, "b")]
printILnFd :: FunDef
printILnFd = builtInFd "printLnI" [(intTn, "i")]

newTcEnv :: TCEnv
newTcEnv = addFuns [printSFd, printBFd, printIFd, printSLnFd, printBLnFd, printILnFd] emptyEnv where
    emptyEnv = TCEnv M.empty M.empty Nothing False

addFuns :: [FunDef] -> TCEnvMod
addFuns [] env = env
addFuns (x:xs) env = env { funs = M.insert name fd $ funs $ addFuns xs env } where
    fd@(FunDefin _ _ name _ _) = x

clearVars :: TCEnvMod
clearVars env = env { vars = M.empty }

addVar :: TypeDef -> Ident -> TCEnvMod
addVar td name env = env { vars = M.insert name td (vars env) }

addVars :: [(TypeDef, Ident)] -> TCEnvMod
addVars [] env = env
addVars ((td, name):xs) env = addVar td name $ addVars xs env

setRetType :: RetTypeInfo -> TCEnvMod
setRetType rt env = env { retType = rt }

modLoop :: Bool -> TCEnvMod
modLoop v env = env { inLoop = v }
enterLoop = modLoop True
exitLoop = modLoop False


data Error =
    ErrFunRedef Ident BNFC'Position
    | ErrVarRedef Ident BNFC'Position
    | ErrUndefVar Ident BNFC'Position
    | ErrReadOnlyAssign Ident BNFC'Position
    | ErrTypeMissmatch Ident TypeName TypeName BNFC'Position
    | ErrExpectedArray Ident BNFC'Position
    | ErrUndefFun Ident BNFC'Position
    | ErrInvalidFunArgCount Ident Int Int BNFC'Position
    | ErrInvalidFunArgs Ident [TypeName] [TypeName] BNFC'Position
    | ErrVoidFunResult Ident BNFC'Position
    | ErrUnsuppAdd TypeName TypeName BNFC'Position
    | ErrNonEmptyReturn BNFC'Position
    | ErrEmptyReturn TypeName BNFC'Position
    | ErrNotInLoop BNFC'Position
    deriving (Show)

type TCReader a = ReaderT TCEnv (ExceptT Error Identity) a

type TCResult a = Either Error a

type TCEnvMod = TCEnv -> TCEnv


fatalError m = error $ "tc fatal: " ++ m


runTc :: [FunDef] -> TCResult ()
runTc fs = (runIdentity . runExceptT . runReaderT (runTcReader fs)) newTcEnv where
    runTcReader :: [FunDef] -> TCReader ()
    runTcReader fs = do
        liftEither $ checkFunRedefs fs
        local (addFuns fs) $ tcFunDefs fs


checkFunRedefs :: [FunDef] -> TCResult ()
checkFunRedefs = checkFunRedefs S.empty where
    checkFunRedefs :: S.Set Ident -> [FunDef] -> TCResult ()
    checkFunRedefs s [] = return ()
    checkFunRedefs s ((FunDefin pos _ name _ _):xs) = if S.member name s
        then throwError $ ErrFunRedef name pos
        else checkFunRedefs (S.insert name s) xs


tcFunDefs :: [FunDef] -> TCReader ()
tcFunDefs = foldr ((>>) . tcFunDef) (return ())
tcFunDef :: FunDef -> TCReader ()
tcFunDef (FunDefin _ retTn name params b) = local (setParamVars params . addRetType retTn . exitLoop) $ tcStmtBlock b where
    setParamVars :: [FunParam] -> TCEnvMod
    setParamVars params = addVars (map (\(FunPar _ td name) -> (td, name)) params) . clearVars
    addRetType :: FunRet -> TCEnvMod
    addRetType FRVoid {} = setRetType Nothing
    addRetType (FRType _ tn) = setRetType $ Just tn


tcStmtBlock :: StmtBlock -> TCReader ()
tcStmtBlock (StmtBlck _ bs) = do
    let sfs = getSubFunDefs bs
        sss = getSubStmts bs
    liftEither $ checkFunRedefs sfs
    local (addFuns sfs) $ do
        tcFunDefs sfs
        tcStmts sss


tcStmts :: [Stmt] -> TCReader ()
tcStmts [] = return ()
tcStmts (x:xs) = do
    m <- tcStmt x
    local m $ tcStmts xs

tcStmt :: Stmt -> TCReader TCEnvMod

tcStmt SEmpty {} = return id

tcStmt (SDecl pos td name) = declareVar td name pos

tcStmt (SAssign pos (AVar _ name exp)) = do
    env <- ask
    liftEither $ do
        (tn, tmod) <- getVarType name pos env
        expectTn tn exp env name pos
        checkAssignment tmod name pos
    return id

tcStmt (SArrAssign pos (AArrAcc _ a@(ArrAcc _ name posExp) exp)) = do
    env <- ask
    liftEither $ do
        (tn, tmod) <- getArrAccType a env
        expectTn tn exp env name pos
        checkAssignment tmod name pos
        expectInt posExp env (Ident "index") pos
    return id

tcStmt (SDeclAssign pos decl exp) = tcDeclAssign decl exp

tcStmt (SFunCall pos fc) = do
    env <- ask
    liftEither $ tcFunCall env fc
    return id

tcStmt (SIf pos (IfBr _ exp ss)) = do
    env <- ask
    liftEither $ expectBool exp env (Ident "if-condition") pos
    tcStmt ss
    return id

tcStmt (SIfEl pos (IfElBr _ exp tsb fs)) = do
    env <- ask
    liftEither $ expectBool exp env (Ident "if-else-condition") pos
    tcStmtBlock tsb
    tcStmt fs
    return id

tcStmt (SLoopWhile pos (LWhile _ exp ss)) = do
    env <- ask
    liftEither $ expectBool exp env (Ident "while-condition") pos
    local enterLoop $ tcStmt ss
    return id

tcStmt (SLoopFor pos (LFor _ (AVar _ itName eFrom) eTo ss)) = do
    m <- declareVar (readOnlyTd intTn) itName pos
    env <- ask
    liftEither $ expect2Int eFrom eTo env pos
    local (m . enterLoop) $ tcStmt ss
    return id

tcStmt (SReturn pos) = do
    env <- ask
    case retType env of
        Nothing -> return id
        Just t -> throwError $ ErrEmptyReturn t pos

tcStmt (SReturnVal pos exp) = do
    env <- ask
    case retType env of
        Nothing -> throwError $ ErrNonEmptyReturn pos
        Just t -> do
            liftEither $ expectTn t exp env (Ident "return-value") pos
            return id

tcStmt (SContinue pos) = requireInLoop pos
tcStmt (SBreak pos) = requireInLoop pos

tcStmt (SSubBlock _ sb) = do
    tcStmtBlock sb
    return id

declareVar :: TypeDef -> Ident -> BNFC'Position -> TCReader TCEnvMod
declareVar td name pos = do
    env <- ask
    when (M.member name (vars env)) $ throwError $ ErrVarRedef name pos
    return $ addVar td name

tcDeclAssign :: DeclA -> Exp -> TCReader TCEnvMod
tcDeclAssign decl exp = do
    let ttn = getDeclTn decl
    env <- ask
    liftEither $ expectTn ttn exp env (Ident "decl-assign") $ hasPosition decl
    declSubVar decl
    where
        getTupleTn :: [DeclA] -> [TypeName]
        getTupleTn ds = map getDeclTn ds
        getDeclTn :: DeclA -> TypeName
        getDeclTn (DeclASingl _ (TypeDefin _ tn _) _) = tn 
        getDeclTn (DeclATuple _ ds) = tupleTn $ getTupleTn ds
        declSubVar :: DeclA -> TCReader TCEnvMod
        declSubVar (DeclASingl pos td name) = declareVar td name pos
        declSubVar (DeclATuple _ ds) = declSubVars ds
        declSubVars :: [DeclA] -> TCReader TCEnvMod
        declSubVars [] = return id
        declSubVars (d:ds) = liftM2 (.) (declSubVar d) (declSubVars ds)

requireInLoop :: BNFC'Position -> TCReader TCEnvMod
requireInLoop pos = do
    env <- ask
    unless (inLoop env) $ throwError $ ErrNotInLoop pos
    return id

checkAssignment :: TypeMod -> Ident -> BNFC'Position -> TCResult ()
checkAssignment mod name pos = case mod of
    TMNone {} -> return ()
    TMReadonly {} -> throwError $ ErrReadOnlyAssign name pos


getVarType :: Ident -> BNFC'Position -> TCEnv -> TCResult (TypeName, TypeMod)
getVarType name pos env = case M.lookup name (vars env) of
    Just (TypeDefin _ tn mod) -> return (tn, mod)
    Nothing -> throwError $ ErrUndefVar name pos

getArrAccType :: ArrayAccess -> TCEnv -> TCResult (TypeName, TypeMod)
getArrAccType (ArrAcc pos name _) env = do
    (at, mod) <- getVarType name pos env
    case at of
        (TNArr _ (TArrayType _ st)) -> return (st, mod)
        _ -> throwError $ ErrExpectedArray name pos


tcExp :: Exp -> TCEnv -> TCResult TypeName

tcExp EInt {} env = return intTn
tcExp EString {} env = return stringTn
tcExp EBool {} env = return boolTn

tcExp (EVarRef pos name) env = do
    (tn, _) <- getVarType name pos env
    return tn

tcExp (EArrInit pos (ArrInit _ tn cExp)) env = do
    expectInt cExp env (Ident "new[]") pos
    return $ arrayTn tn

tcExp (EArrConstr pos (ArrConstr _ els)) env = do
    etn <- tcEls els
    return $ arrayTn etn
    where
        tcEls :: [ConstrEl] -> TCResult TypeName
        tcEls [] = fatalError "empty array construction - should be handled by grammar"
        tcEls [ConstrElem _ exp] = tcExp exp env
        tcEls ((ConstrElem elPos exp):xs) = do
            restTn <- tcEls xs
            expectTn restTn exp env (Ident "array construction") elPos
            return restTn

tcExp (ETupleConstr pos (TupleConstr _ els)) env = do
    ts <- mapM (\(ConstrElem _ exp) -> tcExp exp env) els
    return $ tupleTn ts

tcExp (EArrAcc pos a@(ArrAcc _ name indExp)) env = do
    (atn, _) <- getArrAccType a env
    expectInt indExp env (Ident "index") pos
    return atn

tcExp (EFunCall pos fc@(FuncCall _ name _)) env = do
    frt <- tcFunCall env fc
    case frt of
        FRVoid {} -> throwError $ ErrVoidFunResult name pos
        FRType _ tn -> return tn

tcExp (EMul pos e1 _ e2) env = do
    expect2Int e1 e2 env pos
    return intTn

tcExp (EAdd pos e1 op e2) env = do
    t1 <- tcExp e1 env
    t2 <- tcExp e2 env
    expectSameTn t1 t2 (Ident "addition") pos
    unless (isValidAdd t1 op) $ throwError $ ErrUnsuppAdd t1 t2 pos
    return t1
    where
        isValidAdd :: TypeName -> AddOp -> Bool
        isValidAdd tn AOPlus {} = compareTypes tn stringTn || compareTypes tn intTn
        isValidAdd tn AOMinus {} = compareTypes tn intTn

tcExp (EComp pos e1 op e2) env
    | isEqComp op = do
        t1 <- tcExp e1 env
        t2 <- tcExp e2 env
        expectSameTn t1 t2 (Ident "equality") pos
        return boolTn
    | otherwise = do
        expect2Int e1 e2 env pos
        return boolTn
    where
        isEqComp :: CompOp -> Bool
        isEqComp COEq {} = True
        isEqComp CONeq {} = True
        isEqComp _ = False

tcExp (EAnd pos e1 e2) env = tcBoolOp e1 e2 env pos
tcExp (EOr pos e1 e2) env = tcBoolOp e1 e2 env pos

tcBoolOp :: Exp -> Exp -> TCEnv -> BNFC'Position -> TCResult TypeName
tcBoolOp e1 e2 env pos = do
    expect2Bool e1 e2 env pos
    return boolTn


tcFunCall :: TCEnv -> FunCall -> TCResult FunRet
tcFunCall env (FuncCall pos name args) = do
    (FunDefin _ frt _ params _) <- case M.lookup name (funs env) of
        Nothing -> throwError $ ErrUndefFun name pos
        Just fd -> return fd
    a <- tcArgs env args
    tcFunArgs (paramTypes params) a
    return frt
    where
        paramTypes :: [FunParam] -> [TypeName]
        paramTypes = map (\(FunPar _ (TypeDefin _ tn _) _) -> tn)
        tcArgs :: TCEnv -> [FunArg] -> TCResult [TypeName]
        tcArgs env = mapM (\(FuncArg _ exp) -> tcExp exp env)
        tcFunArgs :: [TypeName] -> [TypeName] -> TCResult ()
        tcFunArgs pts ats
            | paramCount /= argCount = throwError $ ErrInvalidFunArgCount name paramCount argCount pos
            | otherwise = if all (uncurry compareTypes) $ zip pts ats
                then return ()
                else throwError $ ErrInvalidFunArgs name pts ats pos
            where
                paramCount = length pts
                argCount = length ats


expectSameTn :: TypeName -> TypeName -> Ident -> BNFC'Position -> TCResult ()
expectSameTn expected actual name pos = if compareTypes expected actual
    then return ()
    else throwError $ ErrTypeMissmatch name expected actual pos

expectTn :: TypeName -> Exp -> TCEnv -> Ident -> BNFC'Position -> TCResult ()
expectTn expected exp env name pos = do
    tn <- tcExp exp env
    expectSameTn expected tn name pos

expectBool :: Exp -> TCEnv -> Ident -> BNFC'Position -> TCResult ()
expectBool = expectTn boolTn

expectInt :: Exp -> TCEnv -> Ident -> BNFC'Position -> TCResult ()
expectInt = expectTn intTn

expectTn2 :: TypeName -> Exp -> Exp -> TCEnv -> BNFC'Position -> TCResult ()
expectTn2 expected e1 e2 env pos = do
    expectTn expected e1 env (Ident "left") pos
    expectTn expected e2 env (Ident "right") pos

expect2Int :: Exp -> Exp -> TCEnv -> BNFC'Position -> TCResult ()
expect2Int = expectTn2 intTn

expect2Bool :: Exp -> Exp -> TCEnv -> BNFC'Position -> TCResult ()
expect2Bool = expectTn2 boolTn

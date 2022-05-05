module TypeChecker where

import Control.Monad (guard)
import Control.Monad.Except (ExceptT, runExceptT, MonadError (throwError))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT), ask, local)
import qualified Data.Set as S
import qualified Data.Map as M

import Gram.Abs
import TypeHelper

import Debug.Trace (trace)
{-
1. domyślny return
-}



type FunMap = M.Map Ident FunDef
type VarMap = M.Map Ident TypeDef
type RetTypeInfo = Maybe TypeName
data TCEnv = TCEnv {
    funs :: FunMap,
    vars :: VarMap,
    retType :: RetTypeInfo,
    inLoop :: Bool
    }

emptyTcEnv :: TCEnv
emptyTcEnv = TCEnv M.empty M.empty Nothing False

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
    FunRedef Ident BNFC'Position
    | VarRedef Ident BNFC'Position
    | UndefVar Ident BNFC'Position
    | ReadOnlyAssign Ident BNFC'Position
    | TypeMissmatch Ident TypeName TypeName BNFC'Position
    | ExpectedArray Ident BNFC'Position
    | UndefFun Ident BNFC'Position
    | InvalidFunArgCount Ident Int Int BNFC'Position
    | InvalidFunArgs Ident [TypeName] [TypeName] BNFC'Position
    | VoidFunResult Ident BNFC'Position
    | UnsuppAdd TypeName TypeName BNFC'Position
    | NonEmptyReturn BNFC'Position
    | EmptyReturn TypeName BNFC'Position
    | NotInLoop BNFC'Position
    deriving (Show)

type TCReader a = ReaderT TCEnv (ExceptT Error Identity) a

type TCResult a = Either Error a

type TCEnvMod = TCEnv -> TCEnv


fatalError m = error $ "tc fatal: " ++ m


runTc :: [FunDef] -> TCResult ()
runTc fs = (runIdentity . runExceptT . runReaderT (runTcReader fs)) emptyTcEnv where
    runTcReader :: [FunDef] -> TCReader ()
    runTcReader fs = case checkFunRedefs fs of
        Right _ -> local (addFuns fs) $ tcFunDefs fs
        Left err -> throwError err


checkFunRedefs :: [FunDef] -> TCResult (S.Set Ident)
checkFunRedefs [] = return S.empty
checkFunRedefs ((FunDefin pos _ name _ _):xs) = do
    s <- checkFunRedefs xs
    if S.member name s
        then throwError $ FunRedef name pos
        else return $ S.insert name s


tcFunDefs :: [FunDef] -> TCReader ()
tcFunDefs = foldr ((>>) . tcFunDef) (return ())
tcFunDef :: FunDef -> TCReader ()
tcFunDef (FunDefin _ retTn name params b) = tcStmtBlock b (setParamVars params . addRetType retTn . exitLoop) where
    setParamVars :: [FunParam] -> TCEnvMod
    setParamVars params = addVars (map (\(FunPar _ td name) -> (td, name)) params) . clearVars
    addRetType :: FunRet -> TCEnvMod
    addRetType FRVoid {} = setRetType Nothing
    addRetType (FRType _ tn) = setRetType $ Just tn


tcStmtBlock :: StmtBlock -> TCEnvMod -> TCReader ()
tcStmtBlock (StmtBlck _ bs) stmtMod = do
    _ <- case checkFunRedefs sfs of
        Right _ -> return ()
        Left err -> throwError err
    local (addFuns sfs) $ do
        tcFunDefs sfs
        local stmtMod $ tcStmts sss
    where
        sfs = getSubFunDefs bs
        sss = getSubStmts bs
        getSubFunDefs :: [BlockStmt] -> [FunDef]
        getSubFunDefs ((BSFunDef _ fd):xs) = fd:getSubFunDefs xs
        getSubFunDefs (_:xs) = getSubFunDefs xs
        getSubFunDefs [] = []
        getSubStmts :: [BlockStmt] -> [Stmt]
        getSubStmts ((BSStmt _ s):xs) = s:getSubStmts xs
        getSubStmts (_:xs) = getSubStmts xs
        getSubStmts [] = []


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
    case do
        (tn, tmod) <- getVarType name pos env
        expect tn exp env name pos
        checkAssignment tmod name pos
        of
        Left err -> throwError err
        Right _ -> return id

tcStmt (SArrAssign pos (AArrAcc _ a@(ArrAcc _ name posExp) exp)) = do
    env <- ask
    case do
        (tn, tmod) <- getArrAccType a env
        expect tn exp env name pos
        checkAssignment tmod name pos
        expectInt posExp env (Ident "index") pos
        of
        Right _ -> return id
        Left err -> throwError err

tcStmt (SDeclAssign pos (DeclASingl _ td@(TypeDefin _ tn _) name) exp) = do
    m <- declareVar td name pos
    env <- ask
    case expect tn exp env name pos of
        Right _ -> return m
        Left err -> throwError err
tcStmt (SDeclAssign pos (DeclATuple _ tds) exp) = return id

tcStmt (SFunCall pos fc) = do
    env <- ask
    case tcFunCall env fc of
        Right _ -> return id
        Left err -> throwError err

tcStmt (SIf pos (IfBr _ exp ss)) = do
    env <- ask
    case expectBool exp env (Ident "if-condition") pos of
        Right _ -> do
            tcStmt ss
            return id
        Left err -> throwError err

tcStmt (SIfEl pos (IfElBr _ exp tsb fs)) = do
    env <- ask
    case expectBool exp env (Ident "if-else-condition") pos of
        Right _ -> do
            tcStmtBlock tsb id
            tcStmt fs
            return id
        Left err -> throwError err

tcStmt (SLoopWhile pos (LWhile _ exp ss)) = do
    env <- ask
    case expectBool exp env (Ident "while-condition") pos of
        Right _ -> do
            local enterLoop $ tcStmt ss
            return id
        Left err -> throwError err

tcStmt (SLoopFor pos (LFor _ (AVar _ itName eFrom) eTo ss)) = do
    m <- declareVar (readOnlyTd intTn) itName pos
    env <- ask
    case expect2Int eFrom eTo env pos of
        Right _ -> do
            local (m . enterLoop) $ tcStmt ss
            return id
        Left err -> throwError err

tcStmt (SReturn pos) = do
    env <- ask
    case retType env of
        Nothing -> return id
        Just t -> throwError $ EmptyReturn t pos

tcStmt (SReturnVal pos exp) = do
    env <- ask
    case retType env of
        Nothing -> throwError $ NonEmptyReturn pos
        Just t -> case expect t exp env (Ident "return-value") pos of
            Right _ -> return id
            Left err -> throwError err

tcStmt (SContinue pos) = requireInLoop pos
tcStmt (SBreak pos) = requireInLoop pos

tcStmt (SSubBlock _ sb) = do
    tcStmtBlock sb id
    return id

declareVar :: TypeDef -> Ident -> BNFC'Position -> TCReader TCEnvMod
declareVar td name pos = do
    env <- ask
    if M.member name (vars env)
        then throwError $ VarRedef name pos
        else return $ addVar td name

requireInLoop :: BNFC'Position -> TCReader TCEnvMod
requireInLoop pos = do
    env <- ask
    if inLoop env
        then return id
        else throwError $ NotInLoop pos

checkAssignment :: TypeMod -> Ident -> BNFC'Position -> TCResult ()
checkAssignment mod name pos = case mod of
    TMNone {} -> return ()
    TMReadonly {} -> throwError $ ReadOnlyAssign name pos


getVarType :: Ident -> BNFC'Position -> TCEnv -> TCResult (TypeName, TypeMod)
getVarType name pos env = case M.lookup name (vars env) of
    Just (TypeDefin _ tn mod) -> return (tn, mod)
    Nothing -> throwError $ UndefVar name pos

getArrAccType :: ArrayAccess -> TCEnv -> TCResult (TypeName, TypeMod)
getArrAccType (ArrAcc pos name _) env = do
    (at, mod) <- getVarType name pos env
    case at of
        (TNArr _ (TArrayType _ st)) -> return (st, mod)
        _ -> throwError $ ExpectedArray name pos


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
            expect restTn exp env (Ident "array construction") elPos
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
        FRVoid {} -> throwError $ VoidFunResult name pos
        FRType _ tn -> return tn

tcExp (EMul pos e1 _ e2) env = do
    expect2Int e1 e2 env pos
    return intTn

tcExp (EAdd pos e1 op e2) env = do
    t1 <- tcExp e1 env
    t2 <- tcExp e2 env
    expectSame t1 t2 (Ident "addition") pos
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
        expectSame t1 t2 (Ident "equality") pos
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


tcFunCall :: TCEnv -> FunCall -> TCResult FunRet
tcFunCall env (FuncCall pos name args) = do
    (FunDefin _ frt _ params _) <- case M.lookup name (funs env) of
        Nothing -> throwError $ UndefFun name pos
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
            | paramCount /= argCount = throwError $ InvalidFunArgCount name paramCount argCount pos
            | otherwise = if all (uncurry compareTypes) $ zip pts ats
                then return ()
                else throwError $ InvalidFunArgs name pts ats pos
            where
                paramCount = length pts
                argCount = length ats


expectSame :: TypeName -> TypeName -> Ident -> BNFC'Position -> TCResult ()
expectSame expected actual name pos = if compareTypes expected actual
    then return ()
    else throwError $ TypeMissmatch name expected actual pos

expect :: TypeName -> Exp -> TCEnv -> Ident -> BNFC'Position -> TCResult ()
expect expected exp env name pos = do
    tn <- tcExp exp env
    expectSame expected tn name pos

expectBool :: Exp -> TCEnv -> Ident -> BNFC'Position -> TCResult ()
expectBool = expect boolTn

expectInt :: Exp -> TCEnv -> Ident -> BNFC'Position -> TCResult ()
expectInt = expect intTn

expect2 :: TypeName -> Exp -> Exp -> TCEnv -> BNFC'Position -> TCResult ()
expect2 expected e1 e2 env pos = do
    expect expected e1 env (Ident "left") pos
    expect expected e2 env (Ident "right") pos

expect2Int :: Exp -> Exp -> TCEnv -> BNFC'Position -> TCResult ()
expect2Int = expect2 intTn

expect2Bool :: Exp -> Exp -> TCEnv -> BNFC'Position -> TCResult ()
expect2Bool = expect2 boolTn

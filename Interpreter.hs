module Interpreter where

import qualified Data.Map as M

import Common
import Gram.Abs



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

resolveCall :: FunName -> BlockQName -> CallMap -> Maybe FunDef
resolveCall fn [] m = do
    fm <- M.lookup [] m
    M.lookup fn fm
resolveCall fn (i:is) m = case M.lookup (i:is) m of
    Nothing -> resolveCall fn is m
    Just fm -> M.lookup fn fm


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
    stack :: VarStack
    }

type VarName = Ident
type VarMap = M.Map VarName VarAddress

data IPLocalEnv = IPLEnv {
    blockName :: BlockQName,
    varMap :: VarMap
    }

-- runInterpreter :: [FunDef]

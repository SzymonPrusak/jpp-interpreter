module Interpreter where

import qualified Data.Map as M

import Common
import Gram.Abs



type FunName = Ident
type BlockQName = [Ident]

type FunMap = M.Map FunName FunDef
type CallMap = M.Map BlockQName FunMap


buildCallMap :: [FunDef] -> CallMap
buildCallMap fds = foldr (`buildCallMapF` []) (M.singleton [] (createFunMap fds)) fds where
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
            else M.insert ns (createFunMap sfs) $ foldr (`buildCallMapF` ns) m sfs

        buildCallMapS :: Int -> [Stmt] -> BlockQName -> CallMap -> CallMap
        buildCallMapS _ [] _ m = m
        buildCallMapS bi (s:ss) ns m = case s of
            SSubBlock _ sb -> singleBlock sb
            SIf _ (IfBr _ _ (SSubBlock _ sb)) -> singleBlock sb
            SIfEl _ ifel -> case ifel of 
                IfElBr _ _ tsb (SSubBlock _ fsb) -> buildCallMapB fsb (subBlockName 1) $ buildCallMapB tsb (subBlockName 0) $ restCallMap 2
                IfElBr _ _ tsb _ -> singleBlock tsb
            SLoopWhile _ (LWhile _ _ (SSubBlock _ sb)) -> singleBlock sb
            SLoopFor _ (LFor _ _ _ (SSubBlock _ sb)) -> singleBlock sb
            _ -> restCallMap 0
            where
                restCallMap :: Int -> CallMap
                restCallMap i = buildCallMapS (bi + i) ss ns m
                subBlockName :: Int -> BlockQName
                subBlockName i = Ident (show (bi + i)):ns
                singleBlock :: StmtBlock -> CallMap
                singleBlock sb = buildCallMapB sb (subBlockName 0) $ restCallMap 1

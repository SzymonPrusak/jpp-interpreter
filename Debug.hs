module Debug where

import Data.List (intercalate)
import qualified Data.Map as M

import Gram.Print (printTree)
import Interpreter



printNs :: BlockQName -> String
printNs ns = intercalate ":" $ map printTree ns

printCallMap :: CallMap -> String
printCallMap cm = intercalate ",\n" els where
    els = map (\(ns, funMap) -> "(" ++ printNs ns ++ ", { " ++ printFunMap funMap ++ " })") $ M.toList cm

printFunMap :: FunMap -> String
printFunMap fm = intercalate ",\n    " $ map ((++ "()") . printTree . fst) $ M.toList fm

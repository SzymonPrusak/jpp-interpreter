-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Gram.

module Gram.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Gram.Abs

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Gram.Abs.Ident where
  prt _ (Gram.Abs.Ident i) = doc $ showString i
instance Print (Gram.Abs.PrimType' a) where
  prt i = \case
    Gram.Abs.PTBool _ -> prPrec i 0 (concatD [doc (showString "bool")])
    Gram.Abs.PTString _ -> prPrec i 0 (concatD [doc (showString "string")])
    Gram.Abs.PTInt _ -> prPrec i 0 (concatD [doc (showString "int")])

instance Print (Gram.Abs.BoolLit' a) where
  prt i = \case
    Gram.Abs.BTrue _ -> prPrec i 0 (concatD [doc (showString "true")])
    Gram.Abs.BFalse _ -> prPrec i 0 (concatD [doc (showString "false")])

instance Print (Gram.Abs.TypeMod' a) where
  prt i = \case
    Gram.Abs.TMNone _ -> prPrec i 0 (concatD [])
    Gram.Abs.TMReadonly _ -> prPrec i 0 (concatD [doc (showString "readonly")])

instance Print (Gram.Abs.TypeName' a) where
  prt i = \case
    Gram.Abs.TNPrim _ primtype -> prPrec i 0 (concatD [prt 0 primtype])
    Gram.Abs.TNArr _ arraytype -> prPrec i 0 (concatD [prt 0 arraytype])
    Gram.Abs.TNTuple _ tupletype -> prPrec i 0 (concatD [prt 0 tupletype])

instance Print (Gram.Abs.TypeDef' a) where
  prt i = \case
    Gram.Abs.TypeDefin _ typename typemod -> prPrec i 0 (concatD [prt 0 typename, prt 0 typemod])

instance Print (Gram.Abs.ArrayType' a) where
  prt i = \case
    Gram.Abs.TArrayType _ typename -> prPrec i 0 (concatD [prt 0 typename, doc (showString "[]")])

instance Print (Gram.Abs.TupleType' a) where
  prt i = \case
    Gram.Abs.TTupleType _ tuplesubtypes -> prPrec i 0 (concatD [doc (showString "("), prt 0 tuplesubtypes, doc (showString ")")])

instance Print (Gram.Abs.TupleSubType' a) where
  prt i = \case
    Gram.Abs.TupleSType _ typename -> prPrec i 0 (concatD [prt 0 typename])

instance Print [Gram.Abs.TupleSubType' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Gram.Abs.Prog' a) where
  prt i = \case
    Gram.Abs.Program _ fundefs -> prPrec i 0 (concatD [prt 0 fundefs])

instance Print (Gram.Abs.FunDef' a) where
  prt i = \case
    Gram.Abs.FunDefin _ funret id_ funparams stmtblock -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 funret, prt 0 id_, doc (showString "("), prt 0 funparams, doc (showString ")"), prt 0 stmtblock])

instance Print [Gram.Abs.FunDef' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (Gram.Abs.FunRet' a) where
  prt i = \case
    Gram.Abs.FRType _ typename -> prPrec i 0 (concatD [prt 0 typename])
    Gram.Abs.FRVoid _ -> prPrec i 0 (concatD [doc (showString "void")])

instance Print (Gram.Abs.FunParam' a) where
  prt i = \case
    Gram.Abs.FunPar _ typedef id_ -> prPrec i 0 (concatD [prt 0 typedef, prt 0 id_])

instance Print [Gram.Abs.FunParam' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Gram.Abs.StmtBlock' a) where
  prt i = \case
    Gram.Abs.StmtBlck _ blockstmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 blockstmts, doc (showString "}")])

instance Print (Gram.Abs.BlockStmt' a) where
  prt i = \case
    Gram.Abs.BSStmt _ stmt -> prPrec i 0 (concatD [prt 0 stmt])
    Gram.Abs.BSFunDef _ fundef -> prPrec i 0 (concatD [prt 0 fundef])

instance Print [Gram.Abs.BlockStmt' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (Gram.Abs.ArrayInit' a) where
  prt i = \case
    Gram.Abs.ArrInit _ typename exp -> prPrec i 0 (concatD [doc (showString "new"), prt 0 typename, doc (showString "["), prt 0 exp, doc (showString "]")])

instance Print (Gram.Abs.ArrayConstruction' a) where
  prt i = \case
    Gram.Abs.ArrConstr _ constrels -> prPrec i 0 (concatD [doc (showString "["), prt 0 constrels, doc (showString "]")])

instance Print (Gram.Abs.TupleConstruction' a) where
  prt i = \case
    Gram.Abs.TupleConstr _ constrels -> prPrec i 0 (concatD [doc (showString "~("), prt 0 constrels, doc (showString ")")])

instance Print (Gram.Abs.ConstrEl' a) where
  prt i = \case
    Gram.Abs.ConstrElem _ exp -> prPrec i 0 (concatD [prt 0 exp])

instance Print [Gram.Abs.ConstrEl' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Gram.Abs.ArrayAccess' a) where
  prt i = \case
    Gram.Abs.ArrAcc _ id_ exp -> prPrec i 0 (concatD [prt 0 id_, doc (showString "["), prt 0 exp, doc (showString "]")])

instance Print (Gram.Abs.CompOp' a) where
  prt i = \case
    Gram.Abs.COEq _ -> prPrec i 0 (concatD [doc (showString "==")])
    Gram.Abs.CONeq _ -> prPrec i 0 (concatD [doc (showString "!=")])
    Gram.Abs.COGt _ -> prPrec i 0 (concatD [doc (showString ">")])
    Gram.Abs.COLt _ -> prPrec i 0 (concatD [doc (showString "<")])
    Gram.Abs.COGe _ -> prPrec i 0 (concatD [doc (showString ">=")])
    Gram.Abs.COLe _ -> prPrec i 0 (concatD [doc (showString "<=")])

instance Print (Gram.Abs.AddOp' a) where
  prt i = \case
    Gram.Abs.AOPlus _ -> prPrec i 0 (concatD [doc (showString "+")])
    Gram.Abs.AOMinus _ -> prPrec i 0 (concatD [doc (showString "-")])

instance Print (Gram.Abs.MulOp' a) where
  prt i = \case
    Gram.Abs.MOMul _ -> prPrec i 0 (concatD [doc (showString "*")])
    Gram.Abs.MODiv _ -> prPrec i 0 (concatD [doc (showString "/")])
    Gram.Abs.MOMod _ -> prPrec i 0 (concatD [doc (showString "%")])

instance Print (Gram.Abs.IntLiteral' a) where
  prt i = \case
    Gram.Abs.LInt _ n -> prPrec i 0 (concatD [prt 0 n])
    Gram.Abs.LNInt _ n -> prPrec i 0 (concatD [doc (showString "-"), prt 0 n])

instance Print (Gram.Abs.Exp' a) where
  prt i = \case
    Gram.Abs.EInt _ intliteral -> prPrec i 6 (concatD [prt 0 intliteral])
    Gram.Abs.EString _ str -> prPrec i 6 (concatD [printString str])
    Gram.Abs.EBool _ boollit -> prPrec i 6 (concatD [prt 0 boollit])
    Gram.Abs.EVarRef _ id_ -> prPrec i 6 (concatD [prt 0 id_])
    Gram.Abs.EArrInit _ arrayinit -> prPrec i 6 (concatD [prt 0 arrayinit])
    Gram.Abs.EArrConstr _ arrayconstruction -> prPrec i 6 (concatD [prt 0 arrayconstruction])
    Gram.Abs.ETupleConstr _ tupleconstruction -> prPrec i 6 (concatD [prt 0 tupleconstruction])
    Gram.Abs.EArrAcc _ arrayaccess -> prPrec i 6 (concatD [prt 0 arrayaccess])
    Gram.Abs.EFunCall _ funcall -> prPrec i 6 (concatD [prt 0 funcall])
    Gram.Abs.EMul _ exp1 mulop exp2 -> prPrec i 5 (concatD [prt 5 exp1, prt 0 mulop, prt 6 exp2])
    Gram.Abs.EAdd _ exp1 addop exp2 -> prPrec i 4 (concatD [prt 4 exp1, prt 0 addop, prt 5 exp2])
    Gram.Abs.EComp _ exp1 compop exp2 -> prPrec i 3 (concatD [prt 4 exp1, prt 0 compop, prt 4 exp2])
    Gram.Abs.EAnd _ exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "&&"), prt 3 exp2])
    Gram.Abs.EOr _ exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "||"), prt 2 exp2])

instance Print (Gram.Abs.VarAssign' a) where
  prt i = \case
    Gram.Abs.AVar _ id_ exp -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 exp])

instance Print (Gram.Abs.ArrAccAssign' a) where
  prt i = \case
    Gram.Abs.AArrAcc _ arrayaccess exp -> prPrec i 0 (concatD [prt 0 arrayaccess, doc (showString "="), prt 0 exp])

instance Print (Gram.Abs.DeclA' a) where
  prt i = \case
    Gram.Abs.DeclASingl _ typedef id_ -> prPrec i 0 (concatD [prt 0 typedef, prt 0 id_])
    Gram.Abs.DeclATuple _ declas -> prPrec i 0 (concatD [doc (showString "~("), prt 0 declas, doc (showString ")")])

instance Print [Gram.Abs.DeclA' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Gram.Abs.FunCall' a) where
  prt i = \case
    Gram.Abs.FuncCall _ id_ funargs -> prPrec i 0 (concatD [prt 0 id_, doc (showString "("), prt 0 funargs, doc (showString ")")])

instance Print (Gram.Abs.FunArg' a) where
  prt i = \case
    Gram.Abs.FuncArg _ exp -> prPrec i 0 (concatD [prt 0 exp])

instance Print [Gram.Abs.FunArg' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Gram.Abs.IfBranch' a) where
  prt i = \case
    Gram.Abs.IfBr _ exp stmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 stmt])

instance Print (Gram.Abs.IfElseBranch' a) where
  prt i = \case
    Gram.Abs.IfElBr _ exp stmtblock stmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 stmtblock, doc (showString "else"), prt 0 stmt])

instance Print (Gram.Abs.LoopWhile' a) where
  prt i = \case
    Gram.Abs.LWhile _ exp stmt -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 stmt])

instance Print (Gram.Abs.LoopFor' a) where
  prt i = \case
    Gram.Abs.LFor _ varassign exp stmt -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 varassign, doc (showString "to"), prt 0 exp, doc (showString ")"), prt 0 stmt])

instance Print (Gram.Abs.Stmt' a) where
  prt i = \case
    Gram.Abs.SEmpty _ -> prPrec i 0 (concatD [doc (showString ";")])
    Gram.Abs.SDecl _ typedef id_ -> prPrec i 0 (concatD [prt 0 typedef, prt 0 id_, doc (showString ";")])
    Gram.Abs.SAssign _ varassign -> prPrec i 0 (concatD [prt 0 varassign, doc (showString ";")])
    Gram.Abs.SArrAssign _ arraccassign -> prPrec i 0 (concatD [prt 0 arraccassign, doc (showString ";")])
    Gram.Abs.SDeclAssign _ decla exp -> prPrec i 0 (concatD [prt 0 decla, doc (showString "="), prt 0 exp, doc (showString ";")])
    Gram.Abs.SFunCall _ funcall -> prPrec i 0 (concatD [prt 0 funcall, doc (showString ";")])
    Gram.Abs.SIf _ ifbranch -> prPrec i 0 (concatD [prt 0 ifbranch])
    Gram.Abs.SIfEl _ ifelsebranch -> prPrec i 0 (concatD [prt 0 ifelsebranch])
    Gram.Abs.SLoopWhile _ loopwhile -> prPrec i 0 (concatD [prt 0 loopwhile])
    Gram.Abs.SLoopFor _ loopfor -> prPrec i 0 (concatD [prt 0 loopfor])
    Gram.Abs.SReturn _ -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    Gram.Abs.SReturnVal _ exp -> prPrec i 0 (concatD [doc (showString "return"), prt 0 exp, doc (showString ";")])
    Gram.Abs.SContinue _ -> prPrec i 0 (concatD [doc (showString "continue"), doc (showString ";")])
    Gram.Abs.SBreak _ -> prPrec i 0 (concatD [doc (showString "break"), doc (showString ";")])
    Gram.Abs.SSubBlock _ stmtblock -> prPrec i 0 (concatD [prt 0 stmtblock])

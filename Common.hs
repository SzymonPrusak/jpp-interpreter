module Common where

import Gram.Abs


getSubFunDefs :: [BlockStmt] -> [FunDef]
getSubFunDefs ((BSFunDef _ fd):xs) = fd:getSubFunDefs xs
getSubFunDefs (_:xs) = getSubFunDefs xs
getSubFunDefs [] = []

getSubStmts :: [BlockStmt] -> [Stmt]
getSubStmts ((BSStmt _ s):xs) = s:getSubStmts xs
getSubStmts (_:xs) = getSubStmts xs
getSubStmts [] = []
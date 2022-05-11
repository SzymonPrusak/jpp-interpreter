module Main where
    
import Control.Monad.Except (catchError, liftEither)
import Data.List (intercalate)
import System.Environment (getArgs)

import Gram.Abs
import Gram.Par
import Gram.Print
import qualified Interpreter as IP
import qualified TypeChecker as TC

import Debug (printCallMap)



run :: String -> IO ()
run s = do
    p <- runParser s
    case p of
        Just (Program _ fs) -> case TC.runTc fs of
            Right _ ->
                putStr $ printCallMap $ IP.buildCallMap fs
            Left err -> printTcErr err
        Nothing -> return ()

runParser :: String -> IO (Maybe Prog)
runParser s = case pProg $ myLexer s of
    Left err -> do
        putStrLn $ "Parser error: " ++ err
        return Nothing
    Right prog -> return $ Just prog


printTcErr :: TC.Error -> IO ()
printTcErr e = putStrLn $ "Type checker error: " ++ getErrMsg e where
    getErrMsg :: TC.Error -> String 
    getErrMsg e = case e of
        TC.FunRedef name pos -> "Function redefinition: " ++ quote name ++ getPosMsg pos
        TC.VarRedef name pos -> "Variable redefinition: " ++ quote name ++ getPosMsg pos
        TC.UndefVar name pos -> "Undefined variable " ++ quote name ++ getPosMsg pos
        TC.ReadOnlyAssign name pos -> "Assignment to read-only variable " ++ quote name ++ getPosMsg pos
        TC.TypeMissmatch name exp act pos -> "Type missmatch for " ++ quote name ++ " - expected " ++ printTree exp ++ ", but got " ++ printTree act ++ getPosMsg pos
        TC.ExpectedArray name pos -> "Expected array variable: " ++ quote name ++ getPosMsg pos
        TC.UndefFun name pos -> "Undefined function " ++ quote name ++ getPosMsg pos
        TC.InvalidFunArgCount name expC actC pos -> "Invalid argument count for function call - expected " ++ show expC ++ ", but got" ++ show actC ++ getPosMsg pos
        TC.InvalidFunArgs name expA actA pos -> "Invalid arguments for function " ++ quote name ++ " - expected <" ++ printTns expA ++ ">, but got <" ++ printTns actA ++ ">" ++ getPosMsg pos
        TC.VoidFunResult name pos -> "Attempted to get result of void function " ++ quote name ++ getPosMsg pos
        TC.UnsuppAdd t1 t2 pos -> "Unsupported add/sub operator usage on " ++ printTree t1 ++ " and " ++ printTree t2 ++ getPosMsg pos
        TC.NonEmptyReturn pos -> "return <value> in void function" ++ getPosMsg pos
        TC.EmptyReturn exp pos -> "return in non-void function returning " ++ printTree exp ++ getPosMsg pos
        TC.NotInLoop pos -> "Usage of loop control in non-loop body" ++ getPosMsg pos
    quote :: Ident -> String
    quote s = '\"':printTree s ++ "\""
    printTns :: [TypeName] -> String
    printTns = intercalate ", " . map printTree
    getPosMsg :: BNFC'Position -> String
    getPosMsg (Just (line, col)) = " at line " ++ show line ++ ", near column " ++ show col
    getPosMsg Nothing = " at no position given"


main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> readFile path >>= run
        _ -> putStrLn "usage: Call with filename to interpret"
        
module Main where
    
import Control.Monad.Except (catchError, liftEither, MonadIO (liftIO))
import Data.List (intercalate)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import Gram.Abs
import Gram.Par
import Gram.Print
import qualified Interpreter as IP
import qualified TypeChecker as TC



run :: String -> IO ()
run s = do
    p <- runParser s
    case p of
        Just (Program _ fds) -> case TC.runTc fds of
            Right _ -> runInterpreter fds
            Left err -> printTcErr err
        Nothing -> return ()

runParser :: String -> IO (Maybe Prog)
runParser s = case pProg $ myLexer s of
    Left err -> do
        hPutStrLn stderr $ "Parser error: " ++ err
        return Nothing
    Right prog -> return $ Just prog

runInterpreter :: [FunDef] -> IO ()
runInterpreter fds = do
    res <- liftIO $ IP.runInterpreter fds
    case res of
        Right _ -> return ()
        Left err -> printIpError err


printTcErr :: TC.Error -> IO ()
printTcErr e = hPutStrLn stderr $ "Type checker error: " ++ getErrMsg e where
    getErrMsg :: TC.Error -> String 
    getErrMsg e = case e of
        TC.ErrFunRedef name pos -> "Function redefinition: " ++ quote name ++ getPosMsg pos
        TC.ErrVarRedef name pos -> "Variable redefinition: " ++ quote name ++ getPosMsg pos
        TC.ErrUndefVar name pos -> "Undefined variable " ++ quote name ++ getPosMsg pos
        TC.ErrReadOnlyAssign name pos -> "Assignment to read-only variable " ++ quote name ++ getPosMsg pos
        TC.ErrTypeMissmatch name exp act pos -> "Type missmatch for " ++ quote name ++ " - expected " ++ printTree exp ++ ", but got " ++ printTree act ++ getPosMsg pos
        TC.ErrExpectedArray name pos -> "Expected array variable: " ++ quote name ++ getPosMsg pos
        TC.ErrUndefFun name pos -> "Undefined function " ++ quote name ++ getPosMsg pos
        TC.ErrInvalidFunArgCount name expC actC pos -> "Invalid argument count for function " ++ quote name ++ " - expected " ++ show expC ++ ", but got " ++ show actC ++ getPosMsg pos
        TC.ErrInvalidFunArgs name expA actA pos -> "Invalid arguments for function " ++ quote name ++ " - expected <" ++ printTns expA ++ ">, but got <" ++ printTns actA ++ ">" ++ getPosMsg pos
        TC.ErrVoidFunResult name pos -> "Attempted to get result of void function " ++ quote name ++ getPosMsg pos
        TC.ErrUnsuppAdd t1 t2 pos -> "Unsupported add/sub operator usage on " ++ printTree t1 ++ " and " ++ printTree t2 ++ getPosMsg pos
        TC.ErrNonEmptyReturn pos -> "return <value> in void function" ++ getPosMsg pos
        TC.ErrEmptyReturn exp pos -> "return in non-void function returning " ++ printTree exp ++ getPosMsg pos
        TC.ErrNotInLoop pos -> "Usage of loop control in non-loop body" ++ getPosMsg pos
    printTns :: [TypeName] -> String
    printTns = intercalate ", " . map printTree

printIpError :: IP.RuntimeException -> IO ()
printIpError e = hPutStrLn stderr $ "Runtime exception: " ++ getExcMsg e where
    getExcMsg :: IP.RuntimeException -> String
    getExcMsg e = case e of
        IP.ExcEntryPointNotFound -> "Entry point not found"
        IP.ExcVarNotInitialized name pos -> "Usage of not initialized variable " ++ quote name ++ getPosMsg pos
        IP.ExcDivideByZero pos -> "Divide by zero" ++ getPosMsg pos
        IP.ExcIndexOutOfBounds name pos -> "Index out of bounds - variable " ++ quote name ++ getPosMsg pos
        IP.ExcNegativeArrSize pos -> "Negative array size" ++ getPosMsg pos

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
        [] -> getContents >>= run
        [path] -> readFile path >>= run
        _ -> putStrLn "usage: Call with filename to interpret"
        
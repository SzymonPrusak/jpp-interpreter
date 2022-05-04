module Main where
    
import Gram.Par
import System.Environment (getArgs)
import TypeChecker (runTc)
import Gram.Abs (Prog'(Program))


run s =
    case pProg $ myLexer s of
        Left err -> putStrLn $ "error: " ++ err
        Right prog -> do
            putStrLn "Parse success"
            let (Program _ fs) = prog
            case runTc fs of
                Left err2 -> putStrLn $ "tc error: " ++ show err2
                _ -> putStrLn "after parse"


main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> readFile path >>= run
        _ -> putStrLn "usage: Call with filename to interpret"
        
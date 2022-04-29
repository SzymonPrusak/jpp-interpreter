module Main where
    
import Gram.Par
import System.Environment (getArgs)


run s =
    case pProg $ myLexer s of
        Left err -> putStrLn $ "error " ++ err
        Right tree -> do
            putStrLn "Parse success"


main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> readFile path >>= run
        _ -> putStrLn "usage: Call with filename to interpret"
        
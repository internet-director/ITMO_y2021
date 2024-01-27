module Main (main) where

import System.IO
import HW5.Evaluator
import HW5.Pretty
import HW5.Parser(parse)

main :: IO ()
main = loop
    where
        loop = do
            putStr "hi> "
            hFlush stdout
            input <- getLine
            if input /= "exit" then do
                case parse input of
                    Left e -> putStrLn $ show e
                    Right v -> do
                        putStrLn $ show v
                        a <- eval v
                        case a of
                            Left e' -> putStrLn $ show e'
                            Right v' -> putStrLn $ show $ prettyValue v'
                loop
            else
                return ()

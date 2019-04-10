module Main where

import GoatAST
import GoatParser
import GoatPrettyPrinter
import System.Environment
import System.Exit


getParams :: [String] -> (String, String)
getParams [] = ("", "")
getParams (filename:[]) = ("", filename)
getParams (flag:filename:xs) = (flag, filename)

main :: IO ()
main
  = do { progname <- getProgName
        ; args <- getArgs
        ; checkArgs progname args
        ; input <- readFile (last args)
        ; let output = parseGoat input
        ; case output of
            Right ast -> putStr $ prettyprintGoat ast
            --Right ast -> print ast
            Left  err -> do { putStr "Parse error at "
                            ; print err
                            }
        }

checkArgs :: String -> [String] -> IO ()
checkArgs _ [filename]
  = do { putStrLn "Sorry, cannot generate code yet.\n\n"
      ; exitWith (ExitFailure 1)
      }
checkArgs _ ("-p":filename:[])
  = return ()
checkArgs progname _
  = do { putStrLn ("Usage: " ++ progname ++ " [-p] filename\n\n")
      ; exitWith (ExitFailure 1)
      }
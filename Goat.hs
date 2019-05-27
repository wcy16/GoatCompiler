{-|
Module      : Main
Description : Main module for Goat compiler.
Author      : Chunyao Wang

This is the main module for the project. This module is a modification
of the version provided by Professor Sondergaard in the email.
-}
module Main where

import GoatAST
import GoatParser
import GoatPrettyPrinter
import CodeGenerate
import System.Environment
import System.Exit

data Task
  = Parse | Pprint | Compile
    deriving (Show, Eq)

main :: IO ()
main
  = do
      progname <- getProgName
      args <- getArgs
      task <- checkArgs progname args
      if task == Compile then
        do
          let [_, filename] = args
          input <- readFile filename
          let ast = parseGoat input
          case ast of
            Right tree -> putStrLn (generateOzCode tree)
            Left err -> do { putStr "Parse error at "
                            ; print err
                            ; exitWith (ExitFailure 2)
                            }
      else
        if task == Parse then
          do
            let [_, filename] = args
            input <- readFile filename
            let output = parseGoat input
            case output of
              Right tree -> putStrLn (show tree)
              Left err -> do { putStr "Parse error at "
                             ; print err
                             ; exitWith (ExitFailure 2)
                             }
        else
          do
            let [_, filename] = args
            input <- readFile filename
            let output = parseGoat input
            case output of
              Right tree -> putStrLn (prettyprintGoat tree)
              Left err -> do { putStr "Parse error at "
                             ; print err
                             ; exitWith (ExitFailure 2)
                             }

checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_]
  = do
      putStrLn ("Missing filename")
      exitWith (ExitFailure 1)
checkArgs _ [filename]
  = return Compile
checkArgs _ ["-p", filename]
  = return Pprint
checkArgs _ ["-a", filename]
  = return Parse
checkArgs _ ["-c", filename]
  = return Compile
checkArgs progname _
  = do
      putStrLn ("Usage: " ++ progname ++ " [-p] filename")
      exitWith (ExitFailure 1)
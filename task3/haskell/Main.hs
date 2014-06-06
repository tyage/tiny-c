module Main where

import System.Environment
import Text.ParserCombinators.Parsec

import AST
import Parser

run :: String -> String
run input = case parse program "TinyC" input of
            Left err -> show err
            Right val -> show val

main :: IO ()
main = do
  input <- getArgs
  file <- readFile (head input)
  putStrLn (run file)

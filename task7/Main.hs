module Main where

import System.Environment
import Text.ParserCombinators.Parsec
import Control.Monad.State
import Control.Monad.Writer

import Type
import Parser
import Show
import Semantic

run :: String -> String
run input = case parse program "TinyC" input of
            Left err -> show err
            Right program -> show $ execWriterT $ evalStateT (semanticCheck program) initialEnv
              where initialEnv = (Environment (TokensTable Nothing []) 0)

main :: IO ()
main = do
  input <- getArgs
  file <- readFile (head input)
  putStrLn (run file)

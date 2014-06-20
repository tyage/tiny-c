module Main where

import System.Environment
import Text.ParserCombinators.Parsec
import Control.Monad.State
import Control.Monad.Writer

import AST
import Parser
import Show
import Semantic
import CompileError

run :: String -> String
run input = case parseProgram input of
            Left err -> show err
            Right program -> show $ execWriterT $ evalStateT (semanticCheck program) 0

parseProgram :: String -> Either CompileError Program
parseProgram input = case parse program "TinyC" input of
                     Left err -> Left $ ParserError err
                     Right val -> Right val

main :: IO ()
main = do
  input <- getArgs
  file <- readFile (head input)
  putStrLn (run file)

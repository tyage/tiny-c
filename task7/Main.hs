module Main where

import System.Environment
import Text.ParserCombinators.Parsec
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import Data.List

import Type
import Parser
import Show
import Semantic

run :: String -> String
run input = case parse program "TinyC" input of
            Left err -> show err
            Right program -> errorMessages ++ "\n" ++ programTree
              where
                programTree = show $ fst $ result
                errorMessages = concat $ intersperse "\n" $ map show $ snd $ result
                result = fromJust $ runWriterT $ evalStateT (semanticCheck program) initialEnv
                initialEnv = (Environment (TokensTable Nothing []))

main :: IO ()
main = do
  input <- getArgs
  file <- readFile (head input)
  putStrLn (run file)

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

compile :: String -> String -> IO ()
compile filename input = case parse program "TinyC" input of
  Left err -> print err
  Right program -> do
    when (isNothing errorFound) (writeFile filename "a")
    print errorMessages
      where
        programTree = fst $ result
        errorMessages = concat $ intersperse "\n" $ map show $ snd $ result
        errorFound = find isError $ snd result
        result = fromJust $ runWriterT $ evalStateT (semanticCheck program) initialEnv
        initialEnv = (Environment (TokensTable Nothing []))

isError :: ErrorMessage -> Bool
isError (ErrorMessage e) = True
isError (WarningMessage w) = False

main :: IO ()
main = do
  input <- getArgs
  file <- readFile (head input)
  compile "sample.asm" file

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
import Asm

compile :: String -> String -> IO ()
compile filename input = case parse program "TinyC" input of
  Left err -> putStrLn $ show err
  Right program -> do
    putStrLn $ show errorMessages
    when (isNothing errorFound) (writeFile filename asm)
    -- XXX デバッグ用に一時的に標準出力に出す
    putStrLn asm
      where
        asm = concat $ intersperse "\n" $ map show $ asmProgram programTree
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

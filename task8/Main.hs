module Main where

import System.Environment
import System.IO
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

compile :: String -> IO ()
compile input = case parse program "TinyC" input of
  Left err -> hPutStr stderr $ show err
  Right program -> do
    hPutStr stderr errorMessages
    when (isNothing errorFound) (putStrLn asm)
      where
        asm = concat $ intersperse "\n" $ map show $ evalState (asmProgram programTree) initialAsmEnv
        initialAsmEnv = (AsmEnvironment 0 "")
        programTree = fst $ result
        errorMessages = concat $ map (\ x -> show x ++ "\n") $ snd $ result
        errorFound = find isError $ snd result
        result = fromJust $ runWriterT $ evalStateT (semanticCheck program) initialEnv
        initialEnv = (Environment (TokensTable Nothing []))

isError :: ErrorMessage -> Bool
isError (ErrorMessage e) = True
isError (WarningMessage w) = False

main :: IO ()
main = do
  input <- getContents
  compile input

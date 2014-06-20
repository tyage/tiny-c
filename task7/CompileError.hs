module CompileError where

import Text.ParserCombinators.Parsec
import Control.Monad.State
import Control.Monad.Writer

type ErrorChecker a = StateT Int (WriterT [String] Maybe) a

data CompileError = ParserError ParseError
                  | SemanticError [Char]
                  deriving (Show)

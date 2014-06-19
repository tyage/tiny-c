module CompileError where

import Text.ParserCombinators.Parsec

type ErrorChecker a = Either CompileError a

data CompileError = ParserError ParseError
                  | SemanticError [Char]
                  deriving (Show)

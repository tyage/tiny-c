module Type where

import Control.Monad.Writer
import Control.Monad.State

data Program = ExDeclList [ExternalDeclaration]

data ExternalDeclaration = Decl Declaration
                         | FuncDef FunctionDefinition

data Declaration = Declaration DeclaratorList
                 | EmptyDeclaration

data DeclaratorList = DeclaratorList [Declarator]

data Declarator = Declarator Identifier

data FunctionDefinition = FunctionDefinition Declarator ParameterTypeList CompoundStatement

data ParameterTypeList = ParameterTypeList [ParameterDeclaration]

data ParameterDeclaration = ParameterDeclaration Declarator

data Statement = EmptyStatement
               | ExpressionStmt Expr
               | CompoundStmt CompoundStatement
               | If Expr Statement Statement
               | While Expr Statement
               | Return Expr

data CompoundStatement = CompoundStatement DeclarationList StatementList

data DeclarationList = DeclarationList [Declaration]

data StatementList = StatementList [Statement]

data Expr = ExprList [Expr]
          | Assign Identifier Expr
          | Or Expr Expr
          | And Expr Expr
          | Equal Expr Expr
          | NotEqual Expr Expr
          | Lt Expr Expr
          | Gt Expr Expr
          | Le Expr Expr
          | Ge Expr Expr
          | Plus Expr Expr
          | Minus Expr Expr
          | Multiple Expr Expr
          | Divide Expr Expr
          | UnaryMinus Expr
          | FunctionCall Identifier ArgumentExprList
          | Ident Identifier
          | Const Constant
          | Parens Expr

data ArgumentExprList = ArgumentExprList [Expr]

data Identifier = Identifier String
                | TokenIdentifier Token
                deriving (Eq)

data Token = VariableToken Identifier Level Offset
           | ParameterToken Identifier Level Offset
           | FunctionToken Identifier Level ParameterLength
           | FreshToken
           deriving (Eq)

type Offset = Int

type ParameterLength = Int

data Constant = Constant Integer

type ErrorChecker a = StateT Environment (WriterT [String] Maybe) a

data Environment = Environment {
  tokensTable :: TokensTable
}

data TokensTable = TokensTable {
  parentTokensTable :: (Maybe TokensTable),
  tokensList :: [(Identifier, Token)]
}

type Level = Int

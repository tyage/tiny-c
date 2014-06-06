module AST where

data Program = ExDeclList [ExternalDeclaration]
             deriving (Show)

data ExternalDeclaration = Decl Declaration
                         | FuncDef FunctionDefinition
                         deriving (Show)

data Declaration = Declaration DeclaratorList
                 | EmptyDeclaration
                 deriving (Show)

data DeclaratorList = DeclaratorList [Declarator]
                    deriving (Show)

data Declarator = Declarator Identifier
                deriving (Show)

data FunctionDefinition = FunctionDefinition Declarator ParameterTypeList CompoundStatement
                        deriving (Show)

data ParameterTypeList = ParameterTypeList [ParameterDeclaration]
                       deriving (Show)

data ParameterDeclaration = ParameterDeclaration Declarator
                          deriving (Show)

data Statement = EmptyStatement
               | ExpressionStmt Expr
               | CompoundStmt CompoundStatement
               | If Expr Statement Statement
               | While Expr Statement
               | Return Expr
               deriving (Show)

data CompoundStatement = CompoundStatement DeclarationList StatementList
                       deriving (Show)

data DeclarationList = DeclarationList [Declaration]
                     deriving (Show)

data StatementList = StatementList [Statement]
                   deriving (Show)

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
          deriving (Show)

data ArgumentExprList = ArgumentExprList [Expr]
                      deriving (Show)

data Identifier = Identifier String
                deriving (Show)

data Constant = Constant Integer
              deriving (Show)

{-
showExpr :: String -> Expr -> Expr -> String
showExpr s e1 e2 = "(" ++ s ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"

instance Show Expr where
  show (Const i) = show i
  show (Mul e1 e2) = showExpr "*" e1 e2
  show (Div e1 e2) = showExpr "/" e1 e2
  show (Plus e1 e2) = showExpr "+" e1 e2
  show (Minus e1 e2) = showExpr "-" e1 e2
  show (Lt e1 e2) = showExpr "<" e1 e2
  show (Gt e1 e2) = showExpr ">" e1 e2
  show (Le e1 e2) = showExpr "<=" e1 e2
  show (Ge e1 e2) = showExpr ">=" e1 e2
  show (Equal e1 e2) = showExpr "==" e1 e2
  show (NotEqual e1 e2) = showExpr "!=" e1 e2
  show (And e1 e2) = showExpr "&&" e1 e2
  show (Or e1 e2) = showExpr "||" e1 e2
-}

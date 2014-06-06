import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Control.Applicative ((<$>))
import Control.Monad
import Debug.Trace
import Text.Parsec.Language (javaStyle)

-- TODO: move to another file
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

lexer  = P.makeTokenParser javaStyle
natural = P.natural lexer
reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
parserIdentifier = P.identifier lexer
symbol = P.symbol lexer
whiteSpace = P.whiteSpace lexer
semi = P.semi lexer
parens = P.parens lexer
comma = P.comma lexer

program :: Parser Program
program = ExDeclList <$> externalDeclaration `sepBy` whiteSpace
          <?> "program"

externalDeclaration :: Parser ExternalDeclaration
externalDeclaration = try (Decl <$> declaration)
                      <|> FuncDef <$> functionDefinition
                      <?> "external declaration"

declaration :: Parser Declaration
declaration = do reserved "int"
                 d <- declaratorList
                 semi
                 return (Declaration d)
              <?> "declaration"

declaratorList :: Parser DeclaratorList
declaratorList = DeclaratorList <$> declarator `sepBy` comma
                 <?> "declarator list"

declarator :: Parser Declarator
declarator = Declarator <$> identifier
             <?> "declarator"

functionDefinition :: Parser FunctionDefinition
functionDefinition = do reserved "int"
                        d <- declarator
                        p <- parens parameterTypeList
                        c <- compoundStatement
                        return (FunctionDefinition d p c)
                     <?> "function definition"

parameterTypeList :: Parser ParameterTypeList
parameterTypeList = ParameterTypeList <$> parameterDeclaration `sepBy` comma
                    <?> "parameter type list"

parameterDeclaration :: Parser ParameterDeclaration
parameterDeclaration = do reserved "int"
                          d <- declarator
                          return (ParameterDeclaration d)
                       <?> "parameter declaration"

statement :: Parser Statement
statement = try (do semi
                    return EmptyStatement)
            <|> try (do e <- expression
                        semi
                        return (ExpressionStmt e))
            <|> try (CompoundStmt <$> compoundStatement)
            <|> try (do { reserved "if";
                          e <- parens expression;
                          s1 <- statement;
                          do { reserved "else";
                               s2 <- statement;
                               return (If e s1 s2);
                               }
                          <|> return (If e s1 EmptyStatement);
                        })
            <|> try (do reserved "while"
                        e <- parens expression
                        s <- statement
                        return (While e s))
            <|> do reserved "return"
                   e <- expression
                   semi
                   return (Return e)
            <?> "statement"

compoundStatement :: Parser CompoundStatement
compoundStatement = do symbol "{"
                       d <- declarationList
                       s <- statementList
                       symbol "}"
                       return (CompoundStatement d s)
                    <?> "compound statement"

declarationList :: Parser DeclarationList
declarationList = try (DeclarationList <$> many declaration)
                  <|> (whiteSpace >> return (DeclarationList [EmptyDeclaration]))
                  <?> "declaration list"

statementList :: Parser StatementList
statementList = try (StatementList <$> many statement)
                <|> (whiteSpace >> return (StatementList [EmptyStatement]))
                <?> "statement list"

expression :: Parser Expr
expression = ExprList <$> assignExpr `sepBy` comma
             <?> "expression"

assignExpr :: Parser Expr
assignExpr = try (do i <- identifier
                     symbol "="
                     a <- assignExpr
                     return (Assign i a))
             <|> logicalOrExpr
             <?> "assign expr"

logicalOrExpr :: Parser Expr
logicalOrExpr = buildExpressionParser operator unaryExpr <?> "expression"

operator = [[op "*" Multiple AssocLeft, op "/" Divide AssocLeft]
           ,[op "+" Plus AssocLeft, op "-" Minus AssocLeft]
           ,[op "<" Lt AssocLeft, op ">" Gt AssocLeft,
             op "<=" Le AssocLeft, op ">=" Ge AssocLeft]
           ,[op "==" Equal AssocLeft, op "!=" NotEqual AssocLeft]
           ,[op "&&" And AssocLeft]
           ,[op "||" Or AssocLeft]]
          where
            op s f assoc
              = Infix(do
                        reservedOp s
                        return f
                        <?> "operator")
              assoc

unaryExpr :: Parser Expr
unaryExpr = postfixExpr
            <|> do symbol "-"
                   p <- unaryExpr
                   return (UnaryMinus p)
            <?> "unary expression"

postfixExpr :: Parser Expr
postfixExpr = primaryExpr
              <|> do i <- identifier
                     a <- parens argumentExprList
                     return (FunctionCall i a)
              <?> "postfix expression"

primaryExpr :: Parser Expr
primaryExpr = Ident <$> identifier
              <|> Const <$> constant
              <|> Parens <$> parens expression
              <?> "primary expression"

argumentExprList :: Parser ArgumentExprList
argumentExprList = ArgumentExprList <$> assignExpr `sepBy` comma
                   <?> "argument expression list"

identifier :: Parser Identifier
identifier = Identifier <$> parserIdentifier
             <?> "identifier"

constant :: Parser Constant
constant = Constant <$> natural
           <?> "constant"

run :: String -> String
run input = case parse program "Test" input of
            Left  err -> show err
            Right val -> show val

main :: IO ()
main = do
  putStrLn (run "int foo; int hoge(int a,int b){int a;a = 1;if (a == 2) {a = 3;}};")

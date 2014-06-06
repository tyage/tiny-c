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
               | ExpressionStmt Expression
               | CompoundStmt CompoundStatement
               | If Expression Statement Statement
               | While Expression Statement
               | Return Expression
               deriving (Show)

data CompoundStatement = CompoundStatement DeclarationList StatementList
                       deriving (Show)

data DeclarationList = DeclarationList [Declaration]
                     deriving (Show)

data StatementList = StatementList [Statement]
                   deriving (Show)

data Expression = Const Integer
                deriving (Show)

data Identifier = Identifier String
                deriving (Show)

{-
data Expr = Const Integer
          | Identifier String
          | Mul Expr Expr
          | Div Expr Expr
          | Plus Expr Expr
          | Minus Expr Expr
          | Lt Expr Expr
          | Gt Expr Expr
          | Le Expr Expr
          | Ge Expr Expr
          | Equal Expr Expr
          | NotEqual Expr Expr
          | And Expr Expr
          | Or Expr Expr

data Constant = Constant Integer

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
parserIdentifier = P.identifier lexer
symbol = P.symbol lexer
whiteSpace = P.whiteSpace lexer

program :: Parser Program
program = ExDeclList <$> externalDeclaration `sepBy` whiteSpace
          <?> "program"

externalDeclaration :: Parser ExternalDeclaration
externalDeclaration = try (Decl <$> declaration)
                      <|> FuncDef <$> functionDefinition
                      <?> "external declaration"

declaration :: Parser Declaration
declaration = do string "int"
                 whiteSpace
                 d <- declaratorList
                 string ";"
                 return (Declaration d)
              <?> "declaration"

declaratorList :: Parser DeclaratorList
declaratorList = DeclaratorList <$> declarator `sepBy` (symbol ",")
                 <?> "declarator list"

declarator :: Parser Declarator
declarator = Declarator <$> identifier
             <?> "declarator"

functionDefinition :: Parser FunctionDefinition
functionDefinition = do string "int"
                        whiteSpace
                        d <- declarator
                        p <- between (symbol "(") (symbol ")") parameterTypeList
                        c <- compoundStatement
                        return (FunctionDefinition d p c)
                     <?> "function definition"

parameterTypeList :: Parser ParameterTypeList
parameterTypeList = ParameterTypeList <$> parameterDeclaration `sepBy` (symbol ",")
                    <?> "parameter type list"

parameterDeclaration :: Parser ParameterDeclaration
parameterDeclaration = do string "int"
                          whiteSpace
                          d <- declarator
                          return (ParameterDeclaration d)
                       <?> "parameter declaration"

statement :: Parser Statement
statement = try (do string ";"
                    return EmptyStatement)
            <|> try (do e <- expression
                        string ";"
                        return (ExpressionStmt e))
            <|> try (CompoundStmt <$> compoundStatement)
            <|> try (do { string "if";
                          e <- between (symbol "(") (symbol ")") expression;
                          s1 <- statement;
                          do { string "else";
                               s2 <- statement;
                               return (If e s1 s2);
                               }
                          <|> return (If e s1 EmptyStatement);
                        })
            <|> try (do string "while"
                        e <- between (symbol "(") (symbol ")") expression
                        s <- statement
                        return (While e s))
            <|> try (do string "return"
                        e <- expression
                        string ";"
                        return (Return e))
            <?> "statement"

compoundStatement :: Parser CompoundStatement
compoundStatement = try (do string "{"
                            d <- declarationList
                            s <- statementList
                            string "}"
                            return (CompoundStatement d s))
                    <|> try (do string "{"
                                d <- declarationList
                                string "}"
                                return (CompoundStatement d (StatementList [EmptyStatement])))
                    <|> try (do string "{"
                                s <- statementList
                                string "}"
                                return (CompoundStatement (DeclarationList [EmptyDeclaration]) s))
                    <|> try (do string "{"
                                whiteSpace
                                string "}"
                                return (CompoundStatement
                                  (DeclarationList [EmptyDeclaration]) (StatementList [EmptyStatement])))
                    <?> "compound statement"

declarationList :: Parser DeclarationList
declarationList = DeclarationList <$> many declaration
                  <?> "declaration list"

statementList :: Parser StatementList
statementList = StatementList <$> many statement
                <?> "statement list"

expression :: Parser Expression
expression = Const <$> natural
             <?> "expression"

identifier :: Parser Identifier
identifier = Identifier <$> parserIdentifier
             <?> "identifier"

{-
expression :: Parser Expr
expression = assignExpr
             <|> do e <- expression
                    _ <- string ","
                    a <- assignExpr
                    return a
             <?> "expression"

assignExpr :: Parser Expr
assignExpr = logicalOrExpr
             <|> do i <- identifier
                    _ <- string "="
                    a <- assignExpr
                    return a
             <?> "assign expr"

logicalOrExpr :: Parser Expr
logicalOrExpr = buildExpressionParser operator unaryExpr <?> "expression"

operator = [[op "*" Mul AssocLeft, op "/" Div AssocLeft]
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
            <|> do _ <- string '-'
                   p <- unaryExpr
                   -- minus p
                   return p
            <?> "unary expression"

postfixExpr :: Parser Expr
postfixExpr = primaryExpr
              <|> do name <- identifier
                     arg <- between "(" ")" argumentExprList
                     return arg
              <?> "postfix expression"

primaryExpr :: Parser Expr
primaryExpr = identifier
              <|> constant
              <|> between "(" ")" expression
              <?> "primary expression"

argumentExprList :: Parser Expr
argumentExprList = assignExpr
                   <|> do l <- argumentExprList
                          a <- assignExpr
                          return a
                   <?> "argument expression list"

constant :: Parser Constant
constant = do num <- natural
              return (Const num)
           <?> "constant"
-}

run :: String -> String
run input = case parse program "Test" input of
            Left  err -> show err
            Right val -> show val

main :: IO ()
main = do
  putStrLn (run "int foo; int hoge(int a,int b){if(2){}};")

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Control.Applicative ((<$>))
import Control.Monad

-- TODO: move to another file
data Program = ExDecl ExternalDeclaration
             | ExDeclList Program ExternalDeclaration

instance Show Program where
  show (ExDecl externalDeclaration) = show externalDeclaration
  show (ExDeclList program externalDeclaration) = show program ++ "\n" ++ show externalDeclaration

data ExternalDeclaration = Decl Declaration
                         | FuncDef FunctionDefinition

instance Show ExternalDeclaration where
  show (Decl declaration) = show declaration
  show (FuncDef functionDefinition) = show functionDefinition

data Declaration = Hoge String
instance Show Declaration where
  show (Hoge s) = show s

data FunctionDefinition = Fuga String
instance Show FunctionDefinition where
  show (Fuga s) = show s

{-
data Statement = EmptyStatement
               | Expression Expr
               | If Expr Statement Statement
               | While Expr Statement
               | Return Expr

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

lexer  = P.makeTokenParser(emptyDef)
natural = P.natural lexer
reservedOp = P.reservedOp lexer
parserIdentifier = P.identifier lexer
symbol = P.symbol lexer

program :: Parser Program
program = ExDecl <$> externalDeclaration
          <|> liftM2 ExDeclList program externalDeclaration
          <?> "program"

externalDeclaration :: Parser ExternalDeclaration
externalDeclaration = Decl <$> declaration
                      <|> FuncDef <$> functionDefinition
                      <?> "external declaration"

declaration :: Parser Declaration
declaration = do h <- string "hoge"
                 return (Hoge h)

functionDefinition :: Parser FunctionDefinition
functionDefinition = do h <- string "hoge"
                        return (Fuga h)

{-
declaratorList :: Parser Expr
declaratorList = declarator
                 <|> do l <- declaratorList
                        _ <- string ","
                        d <- declarator
                        return d
                <?> "declarator list"

declarator :: Parser Expr
declarator = identifier
             <?> "declarator"

functionDefinition :: Parser Expr
functionDefinition = do _ <- string "int"
                        d <- declarator
                        p <- between (symbol "(") (symbol ")") parameterTypeList
                        c <- compoundStatement
                        return c
                     <?> "function definition"

parameterTypeList :: Parser Expr
parameterTypeList = parameterDeclaration
                    <|> do l <- parameterTypeList
                           _ <- string ","
                           d <- parameterDeclaration
                           return d
                    <?> "parameter type list"

parameterDeclaration :: Parser Expr
parameterDeclaration = do _ <- string "int"
                          d <- declarator
                          return d
                       <?> "parameter declaration"

statement :: Parser Statement
statement = do s <- string";"
               return EmptyStatement
            <|> do e <- expression
                   _ <- string ";"
                   return (Expression e)
            <|> compoundStatement
            <|> do { _ <- string "if";
                     e <- between (symbol "(") (symbol ")") expression;
                     s1 <- statement;
                     do { _ <- string "else";
                          s2 <- statement;
                          return (If e s1 s2);
                          }
                     <|> return (If e s1 EmptyStatement);
                   }
            <|> do _ <- string "while"
                   e <- between (symbol "(") (symbol ")") expression
                   s <- statement
                   return s
            <|> do _ <- string "return"
                   e <- expression
                   _ <- string ";"
                   return e
            <?> "statement"

compoundStatement :: Parser CompoundStatement
compoundStatement = do _ <- string "{"
                       d <- declarationList
                       s <- statementList
                       _ <- string "}"
                       return s
                    <?> "compound statement"

declarationList :: Parser DeclarationList
declarationList = declaration
                 <|> do l <- declaratorList
                        d <- declaration
                        return d
                 <?> "declarator list"

statementList :: Parser Statement
statementList = statement
                <|> do l <- statementList
                       s <- statement
                       return s
                <?> "statement list"

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

identifier :: Parser Expr
identifier = do name <- parserIdentifier
                return (Identifier name)
             <?> "identifier"

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
  putStrLn (run "hoge")

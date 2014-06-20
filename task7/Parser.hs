module Parser where

import Control.Applicative ((<$>))
import Control.Monad

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Language (javaStyle)

import Type

lexer  = Token.makeTokenParser javaStyle
natural = Token.natural lexer
reservedOp = Token.reservedOp lexer
reserved = Token.reserved lexer
parserIdentifier = Token.identifier lexer
symbol = Token.symbol lexer
whiteSpace = Token.whiteSpace lexer
semi = Token.semi lexer
parens = Token.parens lexer
comma = Token.comma lexer

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
postfixExpr = try (do i <- identifier
                      a <- parens argumentExprList
                      return (FunctionCall i a))
              <|> primaryExpr
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

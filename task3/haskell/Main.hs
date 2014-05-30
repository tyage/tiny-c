import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

-- TODO move to another file
data Expr = Integer
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

lexer  = P.makeTokenParser(emptyDef)
natural = P.natural lexer
reservedOp = P.reservedOp lexer
parserIdentifier = P.identifier lexer

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
            <|> do _ <- char '-'
                   p <- unaryExpr
                   -- minus p
                   return p
            <?> "unary expression"

postfixExpr :: Parser Expr
postfixExpr = primaryExpr
{-
              <|> do name <- identifier
                     arg <- between (symbol "(") (symbol ")") argumentExprList
                     -- function call
                     return arg
-}
              <?> "postfix expression"

primaryExpr :: Parser Expr
primaryExpr = -- identifier
              natural
              -- <|> do between (symbol "(") (symbol ")") expression
              <?> "primary expression"

{-
identifier :: Parser Identifier
identifier = do name <- parserIdentifier
                return name

constant :: Parser Constant
constant = do num <- natural
              return (Constant num)
           <?> "constant"
-}

run :: String -> String
run input = case parse logicalOrExpr "Test" input of
            Left  err -> show err
            Right val -> show val

main :: IO ()
main = do
  putStrLn (run "32 + 443 * 5030   / 123 + 99    - 043")

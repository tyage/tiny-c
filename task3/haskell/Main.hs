import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

-- TODO move to another file
data Expr = Const Integer
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
              do p <- natural
                 return (Const p)
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

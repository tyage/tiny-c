import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

lexer  = P.makeTokenParser(emptyDef)
number = P.natural lexer
parens = P.parens lexer
natural = P.natural lexer
reservedOp = P.reservedOp lexer

operatorSystem :: Parser Integer
operatorSystem = buildExpressionParser operator factor

operator = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
           ,[op "+" (+) AssocLeft,op "-" (-) AssocLeft]]
          where
            op s f assoc
              = Infix(do
                        reservedOp s
                        return f
                      <?> "operator")
              assoc

factor :: Parser Integer
factor = parens operatorSystem
      <|> natural

run :: String -> String
run input = case parse operatorSystem "Test" input of
            Left   err -> show err
            Right  val -> show val

main :: IO ()
main = do
  putStrLn (run "32 + 443 * 5030   / 123 + 99    - 043")

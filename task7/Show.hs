module Show where

import Type

instance Show Program where
  show (ExDeclList e) = unlines . map show $ e

instance Show ExternalDeclaration where
  show (Decl d) = show d
  show (FuncDef f) = show f

instance Show Declaration where
  show (Declaration d) = "(int " ++ show d ++ ")\n"
  show (EmptyDeclaration) = ""

instance Show DeclaratorList where
  show (DeclaratorList d) = unwords . map show $ d

instance Show Declarator where
  show (Declarator i) = show i

instance Show FunctionDefinition where
  show (FunctionDefinition d p c) = "((int " ++ show d ++ ") (" ++ show p ++ ")\n" ++ show c ++ ")"

instance Show ParameterTypeList where
  show (ParameterTypeList p) = unwords . map (\s -> "(" ++ show s ++ ")") $ p

instance Show ParameterDeclaration where
  show (ParameterDeclaration d) = "int " ++ show d

instance Show Statement where
  show (EmptyStatement) = ""
  show (ExpressionStmt e) = "(" ++ show e ++ ")"
  show (CompoundStmt c) = "(" ++ show c ++ ")"
  show (If e s1 s2) = "(if " ++ show e ++ "\n" ++ show s1 ++ show s2 ++ ")\n"
  show (While e s) = "(while " ++ show e ++ " " ++ show s ++ ")\n"
  show (Return e) = "(return " ++ show e ++ ")\n"

instance Show CompoundStatement where
  show (CompoundStatement d s) = show d ++ show s

instance Show DeclarationList where
  show (DeclarationList d) = unwords . map show $ d

instance Show StatementList where
  show (StatementList s) = unwords . map show $ s

showExpr :: String -> Expr -> Expr -> String
showExpr s e1 e2 = "(" ++ s ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"

instance Show Expr where
  show (ExprList e) = unwords . map show $ e
  show (Assign i e) = "(= " ++ show i ++ " " ++ show e ++ ")"
  show (Or e1 e2) = showExpr "||" e1 e2
  show (And e1 e2) = showExpr "&&" e1 e2
  show (Equal e1 e2) = showExpr "==" e1 e2
  show (NotEqual e1 e2) = showExpr "!=" e1 e2
  show (Lt e1 e2) = showExpr "<" e1 e2
  show (Gt e1 e2) = showExpr ">" e1 e2
  show (Le e1 e2) = showExpr "<=" e1 e2
  show (Ge e1 e2) = showExpr ">=" e1 e2
  show (Plus e1 e2) = showExpr "+" e1 e2
  show (Minus e1 e2) = showExpr "-" e1 e2
  show (Multiple e1 e2) = showExpr "*" e1 e2
  show (Divide e1 e2) = showExpr "/" e1 e2
  show (UnaryMinus e) = "(- " ++ show e ++ ")"
  show (FunctionCall i a) = "(call " ++ show i ++ " " ++ show a ++ ")"
  show (Ident i) = show i
  show (Const c) = show c
  show (Parens e) = "(" ++ show e ++ ")"

instance Show ArgumentExprList where
  show (ArgumentExprList e) = unwords . map show $ e

instance Show Identifier where
  show (Identifier s) = s
  show (TokenIdentifier t) = show t

instance Show Token where
  show (VariableToken i l o) = show i ++ ":" ++ show l ++ ":" ++ show o
  show (ParameterToken i l o) = show i ++ ":" ++ show l ++ ":" ++ show o
  show (FunctionToken i l p) = show i ++ ":" ++ show l
  show FreshToken = ""

instance Show Constant where
  show (Constant i) = show i

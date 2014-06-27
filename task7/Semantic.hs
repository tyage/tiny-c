module Semantic where

import Control.Applicative hiding (Const)
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe

import Type
import Show

setTokensTable :: TokensTable -> ErrorChecker ()
setTokensTable v = do
  env <- get
  put $ env {tokensTable = v}

createTokensTable :: ErrorChecker ()
createTokensTable = do
  env <- get
  setTokensTable $ TokensTable (Just $ tokensTable env) []

setCurrentLevel :: Int -> ErrorChecker ()
setCurrentLevel i = do
  env <- get
  put $ env {environmentLevel = i}

lookupToken :: Identifier -> ErrorChecker (Maybe Token)
lookupToken i = do
  env <- get
  return $ lookupTokenInTable i (tokensTable env)

lookupTokenInTable :: Identifier -> TokensTable -> (Maybe Token)
lookupTokenInTable i table = case lookup i $ tokensList $ table of
  Nothing -> parentTokensTable table >>= lookupTokenInTable i
  Just t -> Just t

findToken :: Identifier -> ErrorChecker (Maybe Token)
findToken i = lookup i . tokensList . tokensTable <$> get

putToken :: (Identifier -> Level -> Token) -> Identifier -> ErrorChecker ()
putToken t i = do
  env <- get
  put $ env {tokensTable = putTokenInTable env t i}

putTokenInTable :: Environment -> (Identifier -> Level -> Token) -> Identifier -> TokensTable
putTokenInTable env t i = currentTable {tokensList = newTokensList}
  where
    newTokensList = (tokensList currentTable) ++ [(i, newToken)]
    newToken = t i currentLevel
    currentLevel = environmentLevel env
    currentTable = tokensTable env

-- semantic checker
semanticCheck :: Program -> ErrorChecker Program
semanticCheck p = checkProgram p

checkProgram :: Program -> ErrorChecker Program
checkProgram (ExDeclList e) = do
  setCurrentLevel 0
  ExDeclList <$> mapM checkExternalDeclaration e

checkExternalDeclaration :: ExternalDeclaration -> ErrorChecker ExternalDeclaration
checkExternalDeclaration (Decl d) = Decl <$> checkDeclaration d
checkExternalDeclaration (FuncDef f) = FuncDef <$> checkFunctionDefinition f

checkDeclaration :: Declaration -> ErrorChecker Declaration
checkDeclaration (Declaration d) = Declaration <$> checkDeclaratorList d
checkDeclaration (EmptyDeclaration) = return EmptyDeclaration

checkDeclaratorList :: DeclaratorList -> ErrorChecker DeclaratorList
checkDeclaratorList (DeclaratorList d) = DeclaratorList <$> mapM checkVariableDeclarator d

checkVariableDeclarator:: Declarator -> ErrorChecker Declarator
checkVariableDeclarator (Declarator i) = do
  -- 同一レベルで同名の変数宣言・関数宣言があった場合はエラーを出す
  t <- findToken i
  case t of
    Just (VariableToken i l) -> tell ["redeclaration of '" ++ show i ++ "'"]
    Just (FunctionToken i l) -> tell ["'" ++ show i ++ "' redeclarated as different kind of symbol"]
    _ -> return ()

  -- lookupして、paramter宣言があった場合は警告を出す
  t <- lookupToken i
  case t of
    Just (ParameterToken i l) -> tell ["declaration of '" ++ show i ++ "' shadows a parameter"]
    _ -> return ()

  putToken VariableToken i
  Declarator <$> checkIdentifier i

checkFunctionDefinition :: FunctionDefinition -> ErrorChecker FunctionDefinition
checkFunctionDefinition (FunctionDefinition d p c) = liftM3 FunctionDefinition cd cp cc
  where
    cd = checkFunctionDeclarator d
    cp = checkParameterTypeList p
    cc = checkCompoundStatement c

checkFunctionDeclarator:: Declarator -> ErrorChecker Declarator
checkFunctionDeclarator (Declarator i) = do
  -- 同一レベルで同名の変数宣言があった場合はエラーを出す
  t <- findToken i
  case t of
    Just (VariableToken i l) -> tell ["'" ++ show i ++ "' redeclarated as different kind of symbol"]
    Just (FunctionToken i l) -> tell ["redefinition of '" ++ show i ++ "'"]
    _ -> return ()

  putToken FunctionToken i
  Declarator <$> checkIdentifier i

checkParameterTypeList :: ParameterTypeList -> ErrorChecker ParameterTypeList
checkParameterTypeList (ParameterTypeList p) = do
  env <- get
  setCurrentLevel 1
  createTokensTable
  ParameterTypeList <$> mapM checkParameterDeclaration p

checkParameterDeclaration:: ParameterDeclaration -> ErrorChecker ParameterDeclaration
checkParameterDeclaration (ParameterDeclaration d) = ParameterDeclaration <$> checkParameterDeclarator d

checkParameterDeclarator:: Declarator -> ErrorChecker Declarator
checkParameterDeclarator (Declarator i) = do
  -- 同一レベルで同名のパラメータ宣言があった場合はエラーを出す
  t <- findToken i
  case t of
    Just (ParameterToken i l) -> tell ["redeclaration of '" ++ show i ++ "'"]
    _ -> return ()

  putToken ParameterToken i
  Declarator <$> checkIdentifier i

checkStatement :: Statement -> ErrorChecker Statement
checkStatement EmptyStatement = return EmptyStatement
checkStatement (ExpressionStmt e) = ExpressionStmt <$> checkExpression e
checkStatement (CompoundStmt e) = CompoundStmt <$> checkCompoundStatement e
checkStatement (If e s1 s2) = liftM3 If ce cs1 cs2
  where
    ce = checkExpression e
    cs1 = checkStatement s1
    cs2 = checkStatement s2
checkStatement (While e s) = liftM2 While ce cs
  where
    ce = checkExpression e
    cs = checkStatement s
checkStatement (Return e) = Return <$> checkExpression e

checkCompoundStatement :: CompoundStatement -> ErrorChecker CompoundStatement
checkCompoundStatement (CompoundStatement d s) = do
  env <- get
  setCurrentLevel $ (environmentLevel env) + 1
  createTokensTable
  newCompoundStatement <- liftM2 CompoundStatement cd cs
  setCurrentLevel $ (environmentLevel env)
  return newCompoundStatement
    where
      cd = checkDeclarationList d
      cs = checkStatementList s

checkDeclarationList :: DeclarationList -> ErrorChecker DeclarationList
checkDeclarationList (DeclarationList d) = DeclarationList <$> mapM checkDeclaration d

checkStatementList :: StatementList -> ErrorChecker StatementList
checkStatementList (StatementList s) = StatementList <$> mapM checkStatement s

checkExpression :: Expr -> ErrorChecker Expr
checkExpression (ExprList e) = ExprList <$> mapM checkExpression e
checkExpression (Assign i e) = liftM2 Assign ci ce
  where
    ci = checkIdentifier i
    ce = checkExpression e
checkExpression (Or e1 e2) = liftM2 Or ce1 ce2
  where
    ce1 = checkExpression e1
    ce2 = checkExpression e2
checkExpression (And e1 e2) = liftM2 And ce1 ce2
  where
    ce1 = checkExpression e1
    ce2 = checkExpression e2
checkExpression (Equal e1 e2) = liftM2 Equal ce1 ce2
  where
    ce1 = checkExpression e1
    ce2 = checkExpression e2
checkExpression (NotEqual e1 e2) = liftM2 NotEqual ce1 ce2
  where
    ce1 = checkExpression e1
    ce2 = checkExpression e2
checkExpression (Lt e1 e2) = liftM2 Lt ce1 ce2
  where
    ce1 = checkExpression e1
    ce2 = checkExpression e2
checkExpression (Gt e1 e2) = liftM2 Gt ce1 ce2
  where
    ce1 = checkExpression e1
    ce2 = checkExpression e2
checkExpression (Le e1 e2) = liftM2 Le ce1 ce2
  where
    ce1 = checkExpression e1
    ce2 = checkExpression e2
checkExpression (Ge e1 e2) = liftM2 Ge ce1 ce2
  where
    ce1 = checkExpression e1
    ce2 = checkExpression e2
checkExpression (Plus e1 e2) = liftM2 Plus ce1 ce2
  where
    ce1 = checkExpression e1
    ce2 = checkExpression e2
checkExpression (Minus e1 e2) = liftM2 Minus ce1 ce2
  where
    ce1 = checkExpression e1
    ce2 = checkExpression e2
checkExpression (Multiple e1 e2) = liftM2 Multiple ce1 ce2
  where
    ce1 = checkExpression e1
    ce2 = checkExpression e2
checkExpression (Divide e1 e2) = liftM2 Divide ce1 ce2
  where
    ce1 = checkExpression e1
    ce2 = checkExpression e2
checkExpression (UnaryMinus e) = UnaryMinus <$> checkExpression e
checkExpression (FunctionCall i a) = do
  -- 関数が参照できない場合は警告、変数・パラメータの場合はエラー
  t <- lookupToken i
  case t of
    Just (VariableToken i l) -> tell ["variable '" ++ show i ++ "' is used as function"]
    Just (ParameterToken i l) -> tell ["variable '" ++ show i ++ "' is used as function"]
    Nothing -> tell ["'" ++ show i ++ "' undeclared function"]
    _ -> return ()

  liftM2 FunctionCall ci ca
    where
      ci = checkIdentifier i
      ca = checkArgumentExprList a
checkExpression (Ident i) = do
  -- 変数が参照できない場合・関数である場合はエラー
  t <- lookupToken i
  case t of
    Just (FunctionToken i l) -> tell ["function '" ++ show i ++ "' is used as variable"]
    Nothing -> tell ["'" ++ show i ++ "' undeclared variable"]
    _ -> return ()

  Ident <$> checkIdentifier i
checkExpression (Const c) = Const <$> checkConstant c
checkExpression (Parens e) = Parens <$> checkExpression e

checkArgumentExprList :: ArgumentExprList -> ErrorChecker ArgumentExprList
checkArgumentExprList (ArgumentExprList e) = ArgumentExprList <$> mapM checkExpression e

checkIdentifier :: Identifier -> ErrorChecker Identifier
checkIdentifier = return

checkConstant :: Constant -> ErrorChecker Constant
checkConstant = return

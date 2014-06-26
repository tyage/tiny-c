module Semantic where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe

import Type
import Show

setVariablesTable :: VariablesTable -> ErrorChecker ()
setVariablesTable v = do
  env <- get
  put $ env {variablesTable = v}

createVariablesTable :: ErrorChecker ()
createVariablesTable = do
  env <- get
  setVariablesTable $ VariablesTable (Just $ variablesTable env) []

setCurrentLevel :: Int -> ErrorChecker ()
setCurrentLevel i = do
  env <- get
  put $ env {environmentLevel = i}

-- variable utilities
lookupVariable :: Identifier -> ErrorChecker (Maybe Token)
lookupVariable i = do
  env <- get
  return $ lookupVariableInTable i (variablesTable env)

lookupVariableInTable :: Identifier -> VariablesTable -> (Maybe Token)
lookupVariableInTable i table = case lookup i $ variablesList $ table of
  Nothing -> parentVariablesTable table >>= lookupVariableInTable i
  Just t -> Just t

findVariable :: Identifier -> ErrorChecker (Maybe Token)
findVariable i = lookup i . variablesList . variablesTable <$> get

putVariable :: Identifier -> ErrorChecker ()
putVariable i = do
  env <- get
  put $ env {variablesTable = putVariableInTable env i}

putVariableInTable :: Environment -> Identifier -> VariablesTable
putVariableInTable env i = currentTable {variablesList = newVariablesList}
  where
    newVariablesList = (variablesList currentTable) ++ [(i, newToken)]
    newToken = VariableToken i currentLevel
    currentLevel = environmentLevel env
    currentTable = variablesTable env

putParameter :: Identifier -> ErrorChecker ()
putParameter i = do
  env <- get
  put $ env {variablesTable = putParameterInTable env i}

putParameterInTable :: Environment -> Identifier -> VariablesTable
putParameterInTable env i = currentTable {variablesList = newVariablesList}
  where
    newVariablesList = (variablesList currentTable) ++ [(i, newToken)]
    newToken = ParameterToken i currentLevel
    currentLevel = environmentLevel env
    currentTable = variablesTable env

-- function utilities
lookupFunction :: Identifier -> ErrorChecker (Maybe Token)
lookupFunction i = do
  env <- get
  return $ lookupFunctionInTable i $ functionsTable env

lookupFunctionInTable :: Identifier -> FunctionsTable -> (Maybe Token)
lookupFunctionInTable i table = case lookup i $ functionsList $ table of
  Nothing -> parentFunctionsTable table >>= lookupFunctionInTable i
  Just t -> Just t

findFunction :: Identifier -> ErrorChecker (Maybe Token)
findFunction i = lookup i . functionsList . functionsTable <$> get

putFunction :: Identifier -> ErrorChecker ()
putFunction i = do
  env <- get
  put $ env {functionsTable = putFunctionInTable env i}

putFunctionInTable :: Environment -> Identifier -> FunctionsTable
putFunctionInTable env i = currentTable {functionsList = newFunctionsList}
  where
    newFunctionsList = (functionsList currentTable) ++ [(i, newToken)]
    newToken = FunctionToken i currentLevel
    currentLevel = environmentLevel env
    currentTable = functionsTable env

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
  -- 同一レベルで同名の変数宣言があった場合はエラーを出す
  v <- findVariable i
  when (isJust v) $ tell ["redeclaration of '" ++ show i ++ "'"]

  -- 同一レベルで同名の関数宣言があった場合はエラーを出す
  f <- findFunction i
  when (isJust f) $ tell ["'" ++ show i ++ "' redeclarated as different kind of symbol"]

  -- lookupして、paramter宣言があった場合は警告を出す
  p <- lookupVariable i
  case p of
    Just (ParameterToken i l) -> tell ["declaration of '" ++ show i ++ "' shadows a parameter"]
    _ -> return ()

  putVariable i
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
  v <- findVariable i
  when (isJust v) $ tell ["'" ++ show i ++ "' redeclarated as different kind of symbol"]

  -- 同一レベルで同名の関数宣言があった場合はエラーを出す
  f <- findFunction i
  when (isJust f) $ tell ["redeclaration of '" ++ show i ++ "'"]

  putFunction i
  Declarator <$> checkIdentifier i

checkParameterTypeList :: ParameterTypeList -> ErrorChecker ParameterTypeList
checkParameterTypeList (ParameterTypeList p) = do
  env <- get
  setCurrentLevel 1
  createVariablesTable
  ParameterTypeList <$> mapM checkParameterDeclaration p

checkParameterDeclaration:: ParameterDeclaration -> ErrorChecker ParameterDeclaration
checkParameterDeclaration (ParameterDeclaration d) = ParameterDeclaration <$> checkParameterDeclarator d

checkParameterDeclarator:: Declarator -> ErrorChecker Declarator
checkParameterDeclarator (Declarator i) = do
  -- 同一レベルで同名の変数宣言(パラメータ宣言)があった場合はエラーを出す
  v <- findVariable i
  when (isJust v) $ tell ["redeclaration of '" ++ show i ++ "'"]

  putParameter i
  Declarator <$> checkIdentifier i

checkStatement :: Statement -> ErrorChecker Statement
checkStatement EmptyStatement = return EmptyStatement
checkStatement (ExpressionStmt e) = return $ ExpressionStmt e
checkStatement (CompoundStmt e) = return $ CompoundStmt e
checkStatement (If e s1 s2) = return $ If e s1 s2
checkStatement (While e s) = return $ While e s
checkStatement (Return e) = return $ Return e

checkCompoundStatement :: CompoundStatement -> ErrorChecker CompoundStatement
checkCompoundStatement (CompoundStatement d s) = do
  env <- get
  setCurrentLevel $ (environmentLevel env) + 1
  createVariablesTable
  liftM2 CompoundStatement cd cs
    where
      cd = checkDeclarationList d
      cs = checkStatementList s

checkDeclarationList :: DeclarationList -> ErrorChecker DeclarationList
checkDeclarationList (DeclarationList d) = DeclarationList <$> mapM checkDeclaration d

checkStatementList :: StatementList -> ErrorChecker StatementList
checkStatementList (StatementList d) = return $ StatementList d

checkIdentifier :: Identifier -> ErrorChecker Identifier
checkIdentifier (Identifier s) = do
  Environment variablesTable functionsTable currentLevel <- get
  put $ Environment variablesTable functionsTable currentLevel
  tell [s]
  return (Identifier s)

module Semantic where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe

import Type
import Show

setCurrentLevel :: Int -> ErrorChecker ()
setCurrentLevel i = do
  env <- get
  put $ Environment (variablesTable env) (functionsTable env) i

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
  put $ Environment (putVariableInTable env i) (functionsTable env) (environmentLevel env)

putVariableInTable :: Environment -> Identifier -> VariablesTable
putVariableInTable env i = VariablesTable (parentVariablesTable currentTable) newVariablesList
  where
    newVariablesList = (variablesList currentTable) ++ [(i, newToken)]
    newToken = VariableToken i currentLevel
    currentLevel = environmentLevel env
    currentTable = variablesTable env

putParameter :: Identifier -> ErrorChecker ()
putParameter i = do
  env <- get
  put $ Environment (putParameterInTable env i) (functionsTable env) (environmentLevel env)

putParameterInTable :: Environment -> Identifier -> VariablesTable
putParameterInTable env i = VariablesTable (parentVariablesTable currentTable) newVariablesList
  where
    newVariablesList = (variablesList currentTable) ++ [(i, newToken)]
    newToken = ParameterToken i currentLevel
    currentLevel = environmentLevel env
    currentTable = variablesTable env

-- function utilities
lookupFunction :: Identifier -> ErrorChecker (Maybe Token)
lookupFunction i = do
  env <- get
  return $ lookupFunctionInTable i (functionsTable env)

lookupFunctionInTable :: Identifier -> FunctionsTable -> (Maybe Token)
lookupFunctionInTable i table = case lookup i $ functionsList $ table of
  Nothing -> parentFunctionsTable table >>= lookupFunctionInTable i
  Just t -> Just t

findFunction :: Identifier -> ErrorChecker (Maybe Token)
findFunction i = lookup i . functionsList . functionsTable <$> get

putFunction :: Identifier -> ErrorChecker ()
putFunction i = do
  env <- get
  put $ Environment (variablesTable env) (putFunctionInTable env i) (environmentLevel env)

putFunctionInTable :: Environment -> Identifier -> FunctionsTable
putFunctionInTable env i = FunctionsTable (parentFunctionsTable currentTable) newFunctionsList
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
  if isNothing v then return () else tell ["redeclaration of '" ++ show i ++ "'"]

  -- 同一レベルで同名の関数宣言があった場合はエラーを出す
  f <- findFunction i
  if isNothing f then return () else tell ["'" ++ show i ++ "' redeclarated as different kind of symbol"]

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
  if isNothing v then return () else tell ["'" ++ show i ++ "' redeclarated as different kind of symbol"]

  -- 同一レベルで同名の関数宣言があった場合はエラーを出す
  f <- findFunction i
  if isNothing f then return () else tell ["redeclaration of '" ++ show i ++ "'"]

  putFunction i
  Declarator <$> checkIdentifier i

checkParameterTypeList :: ParameterTypeList -> ErrorChecker ParameterTypeList
checkParameterTypeList (ParameterTypeList p) = ParameterTypeList <$> mapM checkParameterDeclaration p

checkParameterDeclaration = return

checkCompoundStatement :: CompoundStatement -> ErrorChecker CompoundStatement
checkCompoundStatement = return

checkIdentifier :: Identifier -> ErrorChecker Identifier
checkIdentifier (Identifier s) = do
  Environment variablesTable functionsTable currentLevel <- get
  put $ Environment variablesTable functionsTable currentLevel
  tell [s]
  return (Identifier s)

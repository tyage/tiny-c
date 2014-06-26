module Semantic where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State

import Type
import Show

setCurrentLevel :: Int -> ErrorChecker ()
setCurrentLevel i = do
  env <- get
  put $ Environment (variablesTable env) i

lookupVariable :: Identifier -> ErrorChecker (Maybe Token)
lookupVariable i = do
  env <- get
  return $ lookupVariableInTable i (variablesTable env)

lookupVariableInTable :: Identifier -> VariablesTable -> (Maybe Token)
lookupVariableInTable i table = case lookup i $ variablesList $ table of
  Nothing -> parentTable table >>= lookupVariableInTable i
  Just t -> Just t

putVariable :: Identifier -> ErrorChecker ()
putVariable i = do
  env <- get
  put $ Environment (putVariableInTable env i) (environmentLevel env)

putVariableInTable :: Environment -> Identifier -> VariablesTable
putVariableInTable env i = VariablesTable (parentTable currentTable) newVariablesList
  where
    newVariablesList = (variablesList currentTable) ++ [(i, newToken)]
    newToken = VariableToken i currentLevel
    currentLevel = environmentLevel env
    currentTable = variablesTable env

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
  case v of
    Nothing -> return ()
    _ -> tell ["redeclaration of " ++ show i]

  -- 同一レベルで同名の関数宣言があった場合はエラーを出す
  f <- findFunction i
  case f of
    Nothing -> return ()
    _ -> tell [show i ++ "redeclarated as different kind of symbol"]

  putVariable i
  Declarator <$> checkIdentifier i

checkFunctionDefinition :: FunctionDefinition -> ErrorChecker FunctionDefinition
checkFunctionDefinition (FunctionDefinition d p c) = liftM3 FunctionDefinition cd cp cc
  where
    cd = checkFunctionDeclarator "function" d
    cp = checkParameterTypeList p
    cc = checkCompoundStatement c

checkFunctionDeclarator:: Declarator -> ErrorChecker Declarator
checkFunctionDeclarator (Declarator i) = do
  -- 同一レベルで同名の変数宣言があった場合はエラーを出す
  v <- findVariable i
  case v of
    Nothing -> return ()
    _ -> tell [show i ++ "redeclarated as different kind of symbol"]

  -- 同一レベルで同名の関数宣言があった場合はエラーを出す
  f <- findFunction i
  case f of
    Nothing -> return ()
    _ -> tell ["redefinition of " ++ show i]

  putFunction i
  Declarator <$> checkIdentifier i

checkParameterTypeList :: ParameterTypeList -> ErrorChecker ParameterTypeList
checkParameterTypeList (ParameterTypeList p) = ParameterTypeList <$> mapM checkParameterDeclaration p

checkParameterDeclaration = return

checkCompoundStatement :: CompoundStatement -> ErrorChecker CompoundStatement
checkCompoundStatement = return

checkIdentifier :: Identifier -> ErrorChecker Identifier
checkIdentifier (Identifier s) = do
  Environment variablesTable currentLevel <- get
  put $ Environment variablesTable currentLevel
  tell [s]
  return (Identifier s)

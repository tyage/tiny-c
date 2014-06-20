module Semantic where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State

import Type

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
checkDeclaratorList (DeclaratorList d) = DeclaratorList <$> mapM checkDeclarator d

checkDeclarator :: Declarator -> ErrorChecker Declarator
checkDeclarator (Declarator i) = do
  Declarator <$> checkIdentifier i

checkFunctionDefinition :: FunctionDefinition -> ErrorChecker FunctionDefinition
checkFunctionDefinition (FunctionDefinition d p c) = liftM3 FunctionDefinition cd cp cc
  where
    cd = checkDeclarator d
    cp = checkParameterTypeList p
    cc = checkCompoundStatement c

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

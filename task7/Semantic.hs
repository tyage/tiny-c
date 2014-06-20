module Semantic where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer

import AST
import CompileError

semanticCheck :: Program -> ErrorChecker Program
semanticCheck p = checkProgram p

checkProgram :: Program -> ErrorChecker Program
checkProgram (ExDeclList e) = ExDeclList <$> mapM checkExternalDeclaration e

checkExternalDeclaration :: ExternalDeclaration -> ErrorChecker ExternalDeclaration
checkExternalDeclaration (Decl d) = Decl <$> checkDeclaration d
checkExternalDeclaration (FuncDef f) = FuncDef <$> checkFunctionDefinition f

checkDeclaration :: Declaration -> ErrorChecker Declaration
checkDeclaration (Declaration d) = Declaration <$> checkDeclaratorList d
checkDeclaration (EmptyDeclaration) = return EmptyDeclaration

checkDeclaratorList :: DeclaratorList -> ErrorChecker DeclaratorList
checkDeclaratorList (DeclaratorList d) = DeclaratorList <$> mapM checkDeclarator d

checkDeclarator :: Declarator -> ErrorChecker Declarator
checkDeclarator (Declarator i) = Declarator <$> checkIdentifier i

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
  tell [s]
  return (Identifier s)

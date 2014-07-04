module Asm where

import Data.List

import Type
import Show

asmProgram :: Program -> [[String]]
asmProgram (ExDeclList e) = concat $ map asmExternalDeclaration e

asmExternalDeclaration :: ExternalDeclaration -> [[String]]
asmExternalDeclaration (Decl d) = asmGlobalDeclaration d
asmExternalDeclaration (FuncDef f) = asmFunctionDefinition f

asmFunctionDefinition :: FunctionDefinition -> [[String]]
asmFunctionDefinition (FunctionDefinition d p c) = [
    ["GLOBAL", show d]
  ]

asmGlobalDeclaration :: Declaration -> [[String]]
asmGlobalDeclaration (Declaration d) = [
    ["COMMON", show d]
  ]

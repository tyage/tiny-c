module Asm where

import Data.List

import Type
import Show

type Asm = [AsmCode]

data AsmCode = AsmGlobal Label
             | AsmLabel Label
             | AsmCommon Label
             | AsmOp Op

data Op = Op0 String
        | Op1 String String
        | Op2 String String String

type Label = String

instance Show AsmCode where
  show (AsmGlobal l) = "GLOBAL\t" ++ l
  show (AsmLabel l) = l ++ ":"
  show (AsmCommon l) = "COMMON\t" ++ l
  show (AsmOp o) = show o

instance Show Op where
  show (Op0 op) = op
  show (Op1 op arg1) = op ++ "\t" ++ arg1
  show (Op2 op arg1 arg2) = op ++ "\t" ++ arg1 ++ ", " ++ arg2

asmProgram :: Program -> Asm
asmProgram (ExDeclList e) = concat $ map asmExternalDeclaration e

asmExternalDeclaration :: ExternalDeclaration -> Asm
asmExternalDeclaration (Decl d) = asmGlobalDeclaration d
asmExternalDeclaration (FuncDef f) = asmFunctionDefinition f

asmFunctionDefinition :: FunctionDefinition -> Asm
asmFunctionDefinition (FunctionDefinition d p c) = [
    AsmGlobal $ show d,
    AsmLabel $ show d,
    AsmOp $ Op1 "push" "ebp",
    AsmOp $ Op2 "mov" "ebp" "esp",
    AsmOp $ Op2 "sub" "esp" "128"
  ] ++ (asmFunctionBody p c) ++ [
    AsmOp $ Op2 "mov" "esp" "ebp",
    AsmOp $ Op1 "pop" "ebp",
    AsmOp $ Op0 "ret"
  ]

asmGlobalDeclaration :: Declaration -> Asm
asmGlobalDeclaration (Declaration d) = [
    AsmCommon $ show d
  ]

asmFunctionBody :: ParameterTypeList -> CompoundStatement -> Asm
asmFunctionBody p c = []

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
    -- XXX 局所変数の最大値を本来は計算するべき
    AsmOp $ Op2 "sub" "esp" "128"
  ] ++ (asmCompoundStatement c) ++ [
    -- XXX return用のlabel名を {funcName}ret としているが、一意にならない可能性があるので連番にしたほうがよい
    AsmLabel $ show d ++ "ret",
    AsmOp $ Op2 "mov" "esp" "ebp",
    AsmOp $ Op1 "pop" "ebp",
    AsmOp $ Op0 "ret"
  ]

asmGlobalDeclaration :: Declaration -> Asm
asmGlobalDeclaration (Declaration d) = [
    AsmCommon $ show d
  ]

asmCompoundStatement :: CompoundStatement -> Asm
asmCompoundStatement (CompoundStatement d s) = case s of
  (StatementList s) -> concat $ map asmStatement s

asmStatement :: Statement -> Asm
asmStatement EmptyStatement = []
asmStatement (ExpressionStmt e) = asmExpression e
asmStatement (CompoundStmt c) = asmCompoundStatement c
asmStatement (If e s1 s2) = []
asmStatement (While e s) = []
asmStatement (Return e) = []

asmExpression :: Expr -> Asm
asmExpression (ExprList e) = concat $ map asmExpression e
asmExpression (Assign i e) = []
asmExpression (Or e1 e2) = []
asmExpression (And e1 e2) = []
asmExpression (Equal e1 e2) = []
asmExpression (NotEqual e1 e2) = []
asmExpression (Lt e1 e2) = []
asmExpression (Gt e1 e2) = []
asmExpression (Le e1 e2) = []
asmExpression (Ge e1 e2) = []
asmExpression (Plus e1 e2) = []
asmExpression (Minus e1 e2) = []
asmExpression (Multiple e1 e2) = []
asmExpression (Divide e1 e2) = []
asmExpression (UnaryMinus e) = []
asmExpression (FunctionCall i a) = []
asmExpression (Ident i) = []
asmExpression (Const c) = [AsmOp $ Op2 "mov" "eax" $ show c]
asmExpression (Parens e) = []

module Asm where

import Data.List

import Type

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

instance Show Declarator where
  show (Declarator i) = show i

instance Show Identifier where
  show (Identifier s) = s
  show (TokenIdentifier t) = show t

instance Show Token where
  show (VariableToken i l o) = show i
  show (ParameterToken i l o) = show i
  show (FunctionToken i l p) = show i
  show FreshToken = ""

instance Show Constant where
  show (Constant i) = show i

showRegister :: Identifier -> String
showRegister (TokenIdentifier t) = showTokenRegister t

showTokenRegister :: Token -> String
showTokenRegister (VariableToken i l o) = "[ebp" ++ show (o * (-4) - 4) ++ "]"
showTokenRegister (ParameterToken i l o) = "[ebp" ++ show (o * 4 + 8) ++ "]"

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
asmGlobalDeclaration (Declaration d) = concat $ map asmGlobalDeclarator $ declList d
  where
    declList (DeclaratorList d) = d

asmGlobalDeclarator :: Declarator -> Asm
asmGlobalDeclarator (Declarator i) = [AsmCommon $ show i]

asmCompoundStatement :: CompoundStatement -> Asm
asmCompoundStatement (CompoundStatement d s) = case s of
  (StatementList s) -> concat $ map asmStatement s

asmStatement :: Statement -> Asm
asmStatement EmptyStatement = []
asmStatement (ExpressionStmt e) = asmExpression e
asmStatement (CompoundStmt c) = asmCompoundStatement c
-- XXX jump先ラベルを生成しろ！
asmStatement (If e s1 s2) = asmExpression e ++ [AsmOp $ Op2 "cmp" "eax" "1",
  AsmOp $ Op1 "je" "L1", AsmOp $ Op1 "jmp" "L2", AsmLabel "L1"] ++
  asmStatement s1 ++ [AsmLabel "L2"] ++ asmStatement s2
-- XXX jump先ラベルを生成しろ！
asmStatement (While e s) = [AsmLabel "BeginWhile"] ++ asmExpression e ++
  [AsmOp $ Op2 "cmp" "eax" "0", AsmOp $ Op1 "je" "EndWhile"] ++ asmStatement s ++
  [AsmOp $ Op1 "jmp" "BeginWhile", AsmLabel "EndWhile"]
-- XXX retラベルにジャンプしろ！
asmStatement (Return e) = asmExpression e ++ [
    AsmOp $ Op2 "mov" "esp" "ebp",
    AsmOp $ Op1 "pop" "ebp",
    AsmOp $ Op0 "ret"
  ]

asmExpression :: Expr -> Asm
asmExpression (ExprList e) = concat $ map asmExpression e
asmExpression (Assign i e) = asmExpression e ++ [AsmOp $ Op2 "mov" (showRegister i) "eax"]
-- XXX jump先ラベルを生成しろ！
asmExpression (Or e1 e2) = [AsmOp $ Op1 "push" "1"] ++ asmExpression e1 ++
  [AsmOp $ Op2 "cmp" "eax" "1", AsmOp $ Op1 "je" "L"] ++ asmExpression e2 ++
  [AsmOp $ Op2 "cmp" "eax" "1", AsmOp $ Op1 "je" "L",
    AsmOp $ Op1 "pop" "eax", AsmOp $ Op1 "push" "0",
    AsmLabel "L", AsmOp $ Op1 "pop" "eax"]
-- XXX jump先ラベルを生成しろ！
asmExpression (And e1 e2) = [AsmOp $ Op1 "push" "0"] ++ asmExpression e1 ++
  [AsmOp $ Op2 "cmp" "eax" "0", AsmOp $ Op1 "je" "L"] ++ asmExpression e2 ++
  [AsmOp $ Op2 "cmp" "eax" "0", AsmOp $ Op1 "je" "L",
    AsmOp $ Op1 "pop" "eax", AsmOp $ Op1 "push" "1",
    AsmLabel "L", AsmOp $ Op1 "pop" "eax"]
asmExpression (Equal e1 e2) = asmCompare e1 e2 "sete"
asmExpression (NotEqual e1 e2) = asmCompare e1 e2 "setne"
asmExpression (Lt e1 e2) = asmCompare e1 e2 "setl"
asmExpression (Gt e1 e2) = asmCompare e1 e2 "setg"
asmExpression (Le e1 e2) = asmCompare e1 e2 "setle"
asmExpression (Ge e1 e2) = asmCompare e1 e2 "setge"
asmExpression (Plus e1 e2) = asmArithmetic e1 e2 "add"
asmExpression (Minus e1 e2) = asmArithmetic e1 e2 "sub"
asmExpression (Multiple e1 e2) = asmArithmetic e1 e2 "imul"
asmExpression (Divide e1 e2) = asmArithmetic e1 e2 "idiv\tdword"
asmExpression (UnaryMinus e) = asmExpression e ++ [AsmOp $ Op2 "imul" "eax" "-1"]
asmExpression (FunctionCall i a) = asmArgumentList a ++ [AsmOp $ Op1 "call" $ show i,
  AsmOp $ Op2 "add" "esp" $ show $ 4 * (argLength a)]
    where
      argLength (ArgumentExprList e) = length e
asmExpression (Ident i) = [AsmOp $ Op2 "mov" "eax" $ showRegister i]
asmExpression (Const c) = [AsmOp $ Op2 "mov" "eax" $ show c]
asmExpression (Parens e) = asmExpression e

asmArgumentList :: ArgumentExprList -> Asm
asmArgumentList (ArgumentExprList a) = concat $ map asmArgument a

asmArgument :: Expr -> Asm
asmArgument e = asmExpression e ++ [AsmOp $ Op1 "push" "eax"]

asmCompare :: Expr -> Expr -> String -> Asm
asmCompare e1 e2 op = asmRSL e1 e2 ++ [AsmOp $ Op2 "cmp" "eax" "ebx",
  AsmOp $ Op1 op "al", AsmOp $ Op2 "movzx" "eax" "al"]

asmArithmetic :: Expr -> Expr -> String -> Asm
asmArithmetic e1 e2 op = asmRSL e1 e2 ++ [AsmOp $ Op2 op "eax" "ebx"]

asmRSL :: Expr -> Expr -> Asm
asmRSL e1 e2 = asmExpression e2 ++ [AsmOp $ Op1 "push" "eax"] ++
  asmExpression e1 ++ [AsmOp $ Op1 "pop" "ebx"]

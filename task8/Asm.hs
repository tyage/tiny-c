module Asm where

import Data.List

import Type
import Show

asmProgram :: Program -> Asm
asmProgram (ExDeclList e) = concat $ map asmExternalDeclaration e

asmExternalDeclaration :: ExternalDeclaration -> Asm
asmExternalDeclaration (Decl d) = asmGlobalDeclaration d
asmExternalDeclaration (FuncDef f) = asmFunctionDefinition f

asmFunctionDefinition :: FunctionDefinition -> Asm
asmFunctionDefinition (FunctionDefinition d p c) = [
    AsmGlobal $ showGlobal identifier,
    AsmLabel $ showGlobal identifier,
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
  where
    identifier = dec2ident d
    dec2ident (Declarator i) = i

asmGlobalDeclaration :: Declaration -> Asm
asmGlobalDeclaration (Declaration d) = concat $ map asmGlobalDeclarator $ declList d
  where
    declList (DeclaratorList d) = d

asmGlobalDeclarator :: Declarator -> Asm
asmGlobalDeclarator (Declarator i) = [AsmCommon (show i) 4]

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

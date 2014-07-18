module Asm where

import Data.List
import Control.Applicative hiding (Const)
import Control.Monad.State

import Type
import Show

genAsmLabel :: State AsmEnvironment Label
genAsmLabel = do
  env <- get
  put $ env { asmLabelCounter = (asmLabelCounter env) + 1 }
  return $ "L" ++ show (asmLabelCounter env)

putReturnLabel :: Label -> State AsmEnvironment ()
putReturnLabel l = do
  env <- get
  put $ env { returnLabel = l }

getReturnLabel :: State AsmEnvironment Label
getReturnLabel = do
  env <- get
  return $ (returnLabel env)

asmProgram :: Program -> Asm
asmProgram (ExDeclList e) = concat <$> mapM asmExternalDeclaration e

asmExternalDeclaration :: ExternalDeclaration -> Asm
asmExternalDeclaration (Decl d) = asmGlobalDeclaration d
asmExternalDeclaration (FuncDef f) = asmFunctionDefinition f

asmFunctionDefinition :: FunctionDefinition -> Asm
asmFunctionDefinition (FunctionDefinition d p c) = do
  retLabel <- genAsmLabel
  putReturnLabel retLabel
  acs <- asmCompoundStatement c
  return $ [
      AsmGlobal $ show identifier,
      AsmLabel $ show identifier,
      AsmOp $ Op1 "push" "ebp",
      AsmOp $ Op2 "mov" "ebp" "esp",
      -- XXX 局所変数の最大値を本来は計算するべき
      AsmOp $ Op2 "sub" "esp" "128"
    ] ++ acs ++ [
      AsmLabel $ retLabel,
      AsmOp $ Op2 "mov" "esp" "ebp",
      AsmOp $ Op1 "pop" "ebp",
      AsmOp $ Op0 "ret"
    ]
    where
      identifier = dec2ident d
      dec2ident (Declarator i) = i

asmGlobalDeclaration :: Declaration -> Asm
asmGlobalDeclaration (Declaration d) = concat <$> mapM asmGlobalDeclarator (declList d)
  where
    declList (DeclaratorList d) = d

asmGlobalDeclarator :: Declarator -> Asm
asmGlobalDeclarator (Declarator i) = return [AsmCommon (show i) 4]

asmCompoundStatement :: CompoundStatement -> Asm
asmCompoundStatement (CompoundStatement d s) = case s of
  (StatementList s) -> concat <$> mapM asmStatement s

asmStatement :: Statement -> Asm
asmStatement EmptyStatement = return []
asmStatement (ExpressionStmt e) = asmExpression e
asmStatement (CompoundStmt c) = asmCompoundStatement c
asmStatement (If e s1 s2) = do
  elseLabel <- genAsmLabel
  endifLabel <- genAsmLabel
  ae <- asmExpression e
  as1 <- asmStatement s1
  as2 <- asmStatement s2
  return $ ae ++ [AsmOp $ Op2 "cmp" "eax" "0", AsmOp $ Op1 "je" elseLabel] ++
    as1 ++ [AsmOp $ Op1 "jmp" endifLabel, AsmLabel elseLabel] ++ as2 ++
    [AsmLabel endifLabel]
asmStatement (While e s) = do
  beginLabel <- genAsmLabel
  endLabel <- genAsmLabel
  ae <- asmExpression e
  as <- asmStatement s
  return $ [AsmLabel beginLabel] ++ ae ++
    [AsmOp $ Op2 "cmp" "eax" "0", AsmOp $ Op1 "je" endLabel] ++ as ++
    [AsmOp $ Op1 "jmp" beginLabel, AsmLabel endLabel]
asmStatement (Return e) = do
  retLabel <- getReturnLabel
  ae <- asmExpression e
  return $ ae ++ [AsmOp $ Op1 "jmp" retLabel]

asmExpression :: Expr -> Asm
asmExpression (ExprList e) = concat <$> mapM asmExpression e
asmExpression (Assign i e) = do
  ae <- asmExpression e
  return $ ae ++ [AsmOp $ Op2 "mov" (showRegister i) "eax"]
asmExpression (Or e1 e2) = do
  orLabel <- genAsmLabel
  ae1 <- asmExpression e1
  ae2 <- asmExpression e2
  return $ [AsmOp $ Op1 "push" "1"] ++ ae1 ++
    [AsmOp $ Op2 "cmp" "eax" "0", AsmOp $ Op1 "jne" orLabel] ++ ae2 ++
    [AsmOp $ Op2 "cmp" "eax" "0", AsmOp $ Op1 "jne" orLabel,
      AsmOp $ Op1 "pop" "eax", AsmOp $ Op1 "push" "0",
      AsmLabel orLabel, AsmOp $ Op1 "pop" "eax"]
asmExpression (And e1 e2) = do
  andLabel <- genAsmLabel
  ae1 <- asmExpression e1
  ae2 <- asmExpression e2
  return $ [AsmOp $ Op1 "push" "0"] ++ ae1 ++
    [AsmOp $ Op2 "cmp" "eax" "0", AsmOp $ Op1 "je" andLabel] ++ ae2 ++
    [AsmOp $ Op2 "cmp" "eax" "0", AsmOp $ Op1 "je" andLabel,
      AsmOp $ Op1 "pop" "eax", AsmOp $ Op1 "push" "1",
      AsmLabel andLabel, AsmOp $ Op1 "pop" "eax"]
asmExpression (Equal e1 e2) = asmCompare e1 e2 "sete"
asmExpression (NotEqual e1 e2) = asmCompare e1 e2 "setne"
asmExpression (Lt e1 e2) = asmCompare e1 e2 "setl"
asmExpression (Gt e1 e2) = asmCompare e1 e2 "setg"
asmExpression (Le e1 e2) = asmCompare e1 e2 "setle"
asmExpression (Ge e1 e2) = asmCompare e1 e2 "setge"
asmExpression (Plus e1 e2) = asmArithmetic e1 e2 "add"
asmExpression (Minus e1 e2) = asmArithmetic e1 e2 "sub"
asmExpression (Multiple e1 e2) = asmArithmetic e1 e2 "imul"
asmExpression (Divide e1 e2) = do
  aRSL <- asmRSL e1 e2
  return $ aRSL ++ [AsmOp $ Op0 "cdq", AsmOp $ Op1 "idiv\tdword" "ecx"]
asmExpression (UnaryMinus e) = do
  ae <- asmExpression e
  return $ ae ++ [AsmOp $ Op2 "imul" "eax" "-1"]
asmExpression (FunctionCall i a) = do
  aal <- asmArgumentList a
  return $ aal ++ extern ++ [AsmOp $ Op1 "call" $ show i,
    AsmOp $ Op2 "add" "esp" $ show $ 4 * (argLength a)]
      where
        extern = if (isUndefined i) then [AsmOp $ Op1 "EXTERN" $ show i] else []
        isUndefined (TokenIdentifier (UndefinedFunctionToken i l p)) = True
        isUndefined _ = False
        argLength (ArgumentExprList e) = length e
asmExpression (Ident i) = return [AsmOp $ Op2 "mov" "eax" $ showRegister i]
asmExpression (Const c) = return [AsmOp $ Op2 "mov" "eax" $ show c]
asmExpression (Parens e) = asmExpression e

asmArgumentList :: ArgumentExprList -> Asm
asmArgumentList (ArgumentExprList a) = concat <$> mapM asmArgument a

asmArgument :: Expr -> Asm
asmArgument e =do
  ae <- asmExpression e
  return $ ae ++ [AsmOp $ Op1 "push" "eax"]

asmCompare :: Expr -> Expr -> String -> Asm
asmCompare e1 e2 op = do
  aRSL <- asmRSL e1 e2
  return $ aRSL ++ [AsmOp $ Op2 "cmp" "eax" "ecx",
    AsmOp $ Op1 op "al", AsmOp $ Op2 "movzx" "eax" "al"]

asmArithmetic :: Expr -> Expr -> String -> Asm
asmArithmetic e1 e2 op = do
  aRSL <- asmRSL e1 e2
  return $ aRSL ++ [AsmOp $ Op2 op "eax" "ecx"]

asmRSL :: Expr -> Expr -> Asm
asmRSL e1 e2 = do
  ae2 <- asmExpression e2
  ae1 <- asmExpression e1
  return $ ae2++ [AsmOp $ Op1 "push" "eax"] ++
    ae1 ++ [AsmOp $ Op1 "pop" "ecx"]

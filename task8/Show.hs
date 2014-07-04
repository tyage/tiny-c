module Show where

import Type

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

instance Show ErrorMessage where
  show (ErrorMessage s) = "error: " ++ s
  show (WarningMessage s) = "warning: " ++ s

instance Show Constant where
  show (Constant i) = show i

instance Show AsmCode where
  show (AsmGlobal l) = "GLOBAL\t" ++ l
  show (AsmLabel l) = l ++ ":"
  show (AsmCommon l b) = "COMMON\t" ++ l ++ show b
  show (AsmOp o) = show o

instance Show Op where
  show (Op0 op) = op
  show (Op1 op arg1) = op ++ "\t" ++ arg1
  show (Op2 op arg1 arg2) = op ++ "\t" ++ arg1 ++ ", " ++ arg2

showRegister :: Identifier -> String
showRegister (TokenIdentifier t) = showTokenRegister t

showTokenRegister :: Token -> String
showTokenRegister (VariableToken i l o) = "[ebp" ++ appendPlus (o * (-4) - 4) ++ "]"
showTokenRegister (ParameterToken i l o) = "[ebp" ++ appendPlus (o * 4 + 8) ++ "]"

showGlobal :: Identifier -> String
showGlobal (TokenIdentifier t) = "_" ++ show t

appendPlus :: Int -> String
appendPlus i = if (i > 0) then "+" ++ show i else show i

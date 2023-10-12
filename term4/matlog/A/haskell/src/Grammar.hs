module Grammar where

import Data.List (intercalate)

data Binop = Impl | Or | And

instance Show Binop where
  show Impl = "->"
  show Or   = "|"
  show And  = "&"

data Expr = Binary Binop Expr Expr
          | Not Expr
          | Var String

instance Show Expr where
  show (Binary op a b) = "(" ++ intercalate "," [show op, show a, show b] ++ ")"
  show (Not e)         = "(!" ++ show e ++ ")"
  show (Var name)      = name

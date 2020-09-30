module Compiler.Syntax.Arithmetic
  ( AExpr (..)
  , ABinOp (..)
  )
where


data AExpr
  = Var String
  | IntConst Integer
  | Neg AExpr
  | Complement AExpr
  | ABinary ABinOp AExpr AExpr
  deriving (Show)

data ABinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

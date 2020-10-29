module Compiler.Syntax.Expression where

data Expr a
  = Var String
  | Const C
  | Binary BinOp (Expr a) (Expr a)
  | Unary UnOp (Expr a)
  deriving (Show)

data C
  = INT Integer
  | CHAR Char
  | BOOL Bool
  deriving (Show, Eq)

data BinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Mod
  | And
  | Or
  | Greater
  | Less
  | Equal
  deriving (Show)

data UnOp
  = Neg
  | Not
  | Complement
  deriving (Show)

module Compiler.Syntax.Control where

import           Compiler.Syntax.Expression

type Name = String

data FParams a =
  Param a Name
  deriving (Show)

data Stmt a
  = Block [Stmt a]
  | Assignment (Assignment a)
  | If (Stmt a) (Stmt a)
  | IfElse (Stmt a) (Stmt a) (Stmt a)
  | Func a Name [FParams a] (Stmt a)
  | Expr (Expr a)
  | Return (Stmt a)
  | Null
  deriving (Show)

data Assignment a
  = Assign a Name (Stmt a)
  | EmptyAssign a Name
  | ValueAssign Name (Stmt a)
  | OpAssign BinOp Name (Stmt a)
  deriving (Show)

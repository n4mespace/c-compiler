module Compiler.Syntax.Control where

import           Compiler.Syntax.Expression

type Name = String

data Stmt a
  = Block [Stmt a]
  | Assignment (Assignment a)
  | If (Stmt a) (Stmt a)
  | IfElse (Stmt a) (Stmt a) (Stmt a)
  | Func (Func a)
  | Expr (Expr a)
  | Return (Stmt a)
  | Null
  deriving (Show)

data Func a
  = CallFunc Name [FArgs a]
  | DeclareFunc a Name [FParams a]
  | DefineFunc a Name [FParams a] (Stmt a)
  deriving (Show)

data FParams a =
  Param a Name
  deriving (Show)

newtype FArgs a =
  Arg (Stmt a)
  deriving (Show)

data Assignment a
  = Assign a Name (Stmt a)
  | EmptyAssign a Name
  | ValueAssign Name (Stmt a)
  | OpAssign BinOp Name (Stmt a)
  deriving (Show)

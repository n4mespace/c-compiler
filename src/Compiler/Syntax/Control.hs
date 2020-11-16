module Compiler.Syntax.Control where

import           Compiler.Syntax.Expression

type Name = String

data Stmt a
  = Block [Stmt a]
  | Assignment (Assignment a)
  | If (Expr a) (Stmt a)
  | IfElse (Expr a) (Stmt a) (Stmt a)
  | Loop (Loop a)
  | Func (Func a)
  | Expr (Expr a)
  | Return (Stmt a)
  -- | Break
  -- | Continue
  | Null
  deriving (Show)

data Func a
  = DeclareFunc a Name [FParam a]
  | DefineFunc a Name [FParam a] (Stmt a)
  deriving (Show)

data FParam a =
  FParam a Name
  deriving (Show)

data Assignment a
  = Assign a Name (Expr a)
  | EmptyAssign a Name
  | ValueAssign Name (Expr a)
  | OpAssign BinOp Name (Expr a)
  deriving (Show)

data Loop a
  = While (Expr a) (Stmt a)
  | For (ForHeader a) (Stmt a)
  deriving (Show)

data ForHeader a =
  ForHeader (Stmt a) (Stmt a) (Stmt a)
  deriving (Show)

instance Eq a => Eq (FParam a) where
  FParam t _ == FParam t' _ = t == t'

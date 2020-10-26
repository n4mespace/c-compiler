module Compiler.Syntax.Control
  ( Expr(..)
  , Stmt(..)
  , FParams(..)
  , Type(..)
  , Name
  ) where

import           Compiler.Syntax.Arithmetic
import           Compiler.Syntax.Boolean

type Name = String

data Type
  = INT
  | CHAR
  | VOID
  | BOOL
  deriving (Show, Eq)

data FParams =
  Param Type Name
  deriving (Show)

data Expr
  = ArExpr AExpr
  | BoolExpr BExpr
  deriving (Show)

data Stmt
  = Block [Stmt]
  | Assign Type Name Stmt
  | EmptyAssign Type Name
  | ValueAssign Name Stmt
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  | Func Type Name [FParams] Stmt
  | Expr Expr
  | Return Stmt
  | Null
  deriving (Show)

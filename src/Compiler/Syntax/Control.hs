module Compiler.Syntax.Control
  ( Expr (..)
  , Stmt (..)
  , FParams (..)
  , Type (..)
  , Name
  )
where


import Compiler.Syntax.Boolean
import Compiler.Syntax.Arithmetic


type Name = String

data Type = INT | CHAR | VOID
  deriving (Show)

data FParams = Param Type Name
  deriving (Show)

data Expr
  = ArExpr AExpr
  | BoolExpr BExpr
  deriving (Show)

data Stmt
  = Block [Stmt]
  | Assign Type Name AExpr
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  | Func Type Name [FParams] Stmt
  | Expr Expr
  | Return Stmt
  | ReturnNull
  deriving (Show)

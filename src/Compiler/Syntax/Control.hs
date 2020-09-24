module Compiler.Syntax.Control
  ( Expr (..)
  , Stmt (..)
  , FParams (..)
  )
where


import Compiler.Syntax.Boolean
import Compiler.Syntax.Arithmetic


type Type = String
type Name = String

data FParams = Param Type Name
  deriving (Show)

data Expr
  = ArExpr AExpr
  | BoolExpr BExpr
  | Null
  deriving (Show)

data Stmt
  = Block [Stmt]
  | Assign Type Name AExpr
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  | Func Type Name [FParams] Stmt
  | Return Expr
  deriving (Show)

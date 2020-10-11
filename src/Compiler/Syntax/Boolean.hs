module Compiler.Syntax.Boolean
  ( BExpr (..)
  , RBinOp (..)
  , BBinOp (..)
  )
where

import           Compiler.Syntax.Arithmetic (AExpr)

data BExpr
  = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr
  deriving (Show)

data BBinOp 
  = And 
  | Or 
  deriving (Show)

data RBinOp 
  = Greater 
  | Less 
  deriving (Show)

{-# LANGUAGE DeriveFunctor #-}

module Compiler.Syntax.Control
  ( Expr(..)
  , Stmt(..)
  , FParams(..)
  , Type(..)
  , Name
  , StmtT
  , FParamsT
  , ExprT
  ) where

import           Compiler.Syntax.Expression

type Name = String

data Type
  = INT_T
  | CHAR_T
  | BOOL_T
  deriving (Show, Eq)

data FParams a =
  Param a Name
  deriving (Show, Functor)

data Stmt a
  = Block [Stmt a]
  | Assign a Name (Stmt a)
  | EmptyAssign a Name
  | ValueAssign Name (Stmt a)
  | If (Expr a) (Stmt a)
  | IfElse (Expr a) (Stmt a) (Stmt a)
  | Func a Name [FParams a] (Stmt a)
  | Expr (Expr a)
  | Return (Stmt a)
  | Null
  deriving (Show, Functor)

type StmtT = Stmt Type

type ExprT = Expr Type

type FParamsT = FParams Type

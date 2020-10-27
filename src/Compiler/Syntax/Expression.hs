{-# LANGUAGE DeriveFunctor #-}

module Compiler.Syntax.Expression
  ( Expr(..)
  , BinOp(..)
  , UnOp(..)
  , C(..)
  ) where

data Expr a
  = Var String
  | Const C
  | Binary BinOp (Expr a) (Expr a)
  | Unary UnOp (Expr a)
  deriving (Show, Functor)

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

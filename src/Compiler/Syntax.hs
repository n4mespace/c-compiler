{-# LANGUAGE OverloadedStrings #-}

module Compiler.Syntax where

import Data.ByteString (ByteString)

--available tokens types
data TokenType =
      OPEN_BRACE
    | CLOSE_BRACE
    | OPEN_PARENTHESIS
    | CLOSE_PARENTHESIS
    | SEMICOLON
    | INT
    | CHAR
    | RETURN
    | VAR
    deriving (Show)

data Token = Token {
  tokenType :: TokenType,
  value     :: ByteString }
  deriving (Show)

--Function parser type
data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving (Show) 


newtype FunctionBody = FunctionBody { 
  exprList :: [Expr] 
  } deriving (Show)


data Function = Function {
  returnType :: ByteString,
  name :: ByteString,
  params :: ByteString,
  body :: FunctionBody }
  deriving (Show)

--values for C-tokens
openBrace :: Char
openBrace = '{'

opBrace :: Token
opBrace = Token OPEN_BRACE "{"

closeBrace :: Char
closeBrace = '}'

openParenthesis :: Char
openParenthesis = '('

closeParenthesis :: Char
closeParenthesis = ')'

semicolon :: Char
semicolon = ';'

int :: ByteString
int = "int"

char :: ByteString
char = "char"

return :: ByteString
return = "return"



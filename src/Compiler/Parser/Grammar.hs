module Compiler.Parser.Grammar where

import           Compiler.Errors               (lexerErr, pretty)
import           Compiler.Parser.Checkers      (runChecker)
import           Compiler.Parser.Common        (initialEnv)
import           Compiler.Types

import           Text.ParserCombinators.Parsec (ParseError)

checkFile :: (Program, StmtT) -> IO (Program, StmtT)
checkFile (p, ast) =
  case runChecker ast (initialEnv p) of
    Left e -> pretty e >> fail "parser error"
    Right checkedAst -> do
      putStrLn "\n{-# GENERATED AST-TOKENS #-}"
      pretty ast
      return (p, checkedAst)

checkString :: (Program, Either ParseError StmtT)
            -> (Program, Either ErrT StmtT)
checkString (p, ast) =
  case ast of
    Left e     -> (p, lexerErr)
    Right ast' -> (p, runChecker ast' $ initialEnv p)

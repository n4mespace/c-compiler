module Compiler.Parser.Grammar where

import           Compiler.Parser.Checkers
import           Compiler.Types

import           Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict                as M
import           Text.Pretty.Simple             (pPrint)

checkGrammar :: StmtT -> IO StmtT
checkGrammar parsedProgram = do
  case checkerProgram of
    Left e -> print e >> fail "parser error"
    Right checkedProgram -> do
      putStrLn "\n{-# GENERATED AST-TOKENS #-}"
      pPrint parsedProgram
      return checkedProgram
  where
    checkerProgram :: Either ErrT StmtT
    checkerProgram = evalStateT (checker parsedProgram) initialState

    initialState :: GlobalEnv
    initialState = (-1, M.singleton (-1, "") 0)

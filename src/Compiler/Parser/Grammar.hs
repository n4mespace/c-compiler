module Compiler.Parser.Grammar where

import           Compiler.Parser.Checkers
import           Compiler.Types

import qualified Data.Map.Strict          as M
import           Text.Pretty.Simple       (CheckColorTty (CheckColorTty),
                                           OutputOptions (..),
                                           defaultOutputOptionsDarkBg,
                                           pPrintOpt)

checkGrammar :: StmtT -> IO StmtT
checkGrammar ast =
  case checkerProgram ast initialEnv of
    Left e -> print e >> fail "parser error"
    Right checkedAst -> do
      putStrLn "\n{-# GENERATED AST-TOKENS #-}"
      pretty ast
      return checkedAst
  where
    initialEnv :: GlobalEnv
    initialEnv = (-1, M.singleton (-1, "") (0, False, []))

    opts :: OutputOptions
    opts = defaultOutputOptionsDarkBg
      { outputOptionsIndentAmount = 4
      , outputOptionsPageWidth = 65
      , outputOptionsCompact = True
      }

    pretty :: StmtT -> IO ()
    pretty = pPrintOpt CheckColorTty opts

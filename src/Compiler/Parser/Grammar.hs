module Compiler.Parser.Grammar where

import           Compiler.Parser.Checkers
import           Compiler.Types

import           Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict                as M
import           Text.Pretty.Simple             (CheckColorTty (CheckColorTty),
                                                 OutputOptions (..),
                                                 defaultOutputOptionsDarkBg,
                                                 pPrintOpt)

checkGrammar :: StmtT -> IO StmtT
checkGrammar parsedProgram = do
  case checkerProgram of
    Left e -> print e >> fail "parser error"
    Right checkedProgram -> do
      putStrLn "\n{-# GENERATED AST-TOKENS #-}"
      pretty parsedProgram
      return checkedProgram
  where
    checkerProgram :: Either ErrT StmtT
    checkerProgram = evalStateT (checker parsedProgram) initialState

    initialState :: GlobalEnv
    initialState = (-1, M.singleton (-1, "") 0)

    opts :: OutputOptions
    opts = defaultOutputOptionsDarkBg
      { outputOptionsIndentAmount = 4
      , outputOptionsPageWidth = 65
      , outputOptionsCompact = True
      }

    pretty :: StmtT -> IO ()
    pretty = pPrintOpt CheckColorTty opts

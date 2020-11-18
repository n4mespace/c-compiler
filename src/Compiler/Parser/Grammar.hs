module Compiler.Parser.Grammar where

import           Compiler.Errors               (lexerErr)
import           Compiler.Parser.Checkers      (runChecker)
import           Compiler.Parser.Common        (initialEnv)
import           Compiler.Types

import           Text.ParserCombinators.Parsec (ParseError)
import           Text.Pretty.Simple            (CheckColorTty (CheckColorTty),
                                                OutputOptions (..),
                                                defaultOutputOptionsDarkBg,
                                                pPrintOpt)

checkFile :: StmtT -> IO StmtT
checkFile ast =
  case runChecker ast initialEnv of
    Left e -> print e >> fail "parser error"
    Right checkedAst -> do
      putStrLn "\n{-# GENERATED AST-TOKENS #-}"
      pretty ast
      return checkedAst
  where
    pretty :: StmtT -> IO ()
    pretty = pPrintOpt CheckColorTty opts

    opts :: OutputOptions
    opts = defaultOutputOptionsDarkBg
      { outputOptionsIndentAmount = 4
      , outputOptionsPageWidth = 65
      , outputOptionsCompact = True
      }

checkString :: Either ParseError StmtT -> Either ErrT StmtT
checkString ast =
  case ast of
    Left e     -> lexerErr
    Right ast' -> runChecker ast' initialEnv

module Lib
  ( main
  , parseFile
  )
where

import           Compiler.Grammar        (checkGrammar)
import           Compiler.Parser
import           Compiler.Syntax.Control (Stmt)
import           Control.Monad           (void)
import           System.IO
import           Text.Parsec.Prim        (parse)
import           Text.Pretty.Simple      (pPrint)

main :: IO ()
main = do
  program <- parseFile "test/lab1.c" >>= checkGrammar
  case program of
    Left e -> print e >> fail "parse error"
    Right r -> do
      putStrLn "\n{-# GENERATED AST-TOKENS #-}"
      pPrint r

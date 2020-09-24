module Lib
  ( main
  , parseFile
  )
where

import           Compiler.Generator.MASM  (generateMASM)
import           Compiler.Grammar        (checkGrammar)
import           Compiler.Parser

import           Text.Pretty.Simple      (pPrint)

main :: IO ()
main = do
  program <- parseFile "test/lab1.c" >>= checkGrammar
  case program of
    Left e -> print e >> fail "parse error"
    Right r -> do
      putStrLn "{-# GENERATED AST-TOKENS #-}"
      pPrint r
  asm <- generateMASM program
  putStrLn "\n{-# GENERATED .ASM #-}"
  putStrLn asm
  writeFile "test/chank.asm" asm

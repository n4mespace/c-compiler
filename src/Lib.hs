module Lib
  ( main
  )
where

import           Compiler.Generator.MASM (generateMASM)
import           Compiler.Grammar        (checkGrammar)
import           Compiler.Parser

import           Text.Pretty.Simple      (pPrint)


main :: FilePath -> FilePath -> IO ()
main filePath generateTo = do
  program <- parseFile filePath
         >>= checkGrammar

  case program of
    Left e -> print e >> fail "parse error"
    Right p -> do
      putStrLn "{-# GENERATED AST-TOKENS #-}"
      pPrint p

  asm <- generateMASM program
  putStrLn "\n{-# GENERATED .ASM #-}"
  putStrLn asm
  writeFile generateTo asm

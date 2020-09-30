module Lib
  ( main
  )
where

import           Compiler.Generator.MASM (generateMASM)
import           Compiler.Grammar        (checkGrammar)
import           Compiler.Parser

import           Text.Pretty.Simple      (pPrint)


main :: FilePath -> FilePath -> IO ()
main filePath destination = do
  program <- parseFile filePath
         >>= checkGrammar

  case program of
    Left e -> print e >> fail "parse error"
    Right parsedProgram -> do
      putStrLn "{-# GENERATED AST-TOKENS #-}"
      pPrint parsedProgram

      asm <- generateMASM parsedProgram
      case asm of
        Left e -> print e >> fail "asm gen error"
        Right generatedASM -> do
          putStrLn "\n{-# GENERATED .ASM #-}"
          putStrLn generatedASM
          destination `writeFile` generatedASM

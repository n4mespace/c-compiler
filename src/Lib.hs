module Lib
  ( compileFile
  , compileString
  ) where

import           Compiler.Generator.MASM (generateFile, generateString)
import           Compiler.Lexer.Parse    (parseFile, parseString)
import           Compiler.Parser.Grammar (checkGrammar)

compileFile :: FilePath -> FilePath -> IO ()
compileFile source destination =
      parseFile source
  >>= checkGrammar
  >>= generateFile destination

compileString :: String -> IO String
compileString source =
      parseString source
  >>= checkGrammar
  >>= generateString

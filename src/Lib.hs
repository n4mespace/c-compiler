module Lib
  ( compileFile
  , compileString
  ) where

import           Compiler.Generator.MASM (generateFile, generateString)
import           Compiler.Grammar        (checkGrammar)
import           Compiler.Parser         (parseFile, parseString)

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

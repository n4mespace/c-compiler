module Lib
  ( main
  ) where

import           Compiler.Generator.MASM (generateMASM)
import           Compiler.Grammar        (checkGrammar)
import           Compiler.Parser

main :: FilePath -> FilePath -> IO ()
main filePath destination =
  parseFile filePath >>= checkGrammar >>= generateMASM destination

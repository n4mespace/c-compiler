module Lib
  ( compileFile
  , compileString
  ) where

import           Compiler.Generator.MASM (generateFile, generateString)
import           Compiler.Lexer.Parse    (parseFile, parseString)
import           Compiler.Parser.Grammar (checkFile, checkString)
import           Compiler.Types          (ErrT)

compileFile :: FilePath -> FilePath -> IO ()
compileFile source destination = parseFile source >>=
                                 checkFile >>=
                                 generateFile destination

compileString :: String -> Either ErrT String
compileString = generateString . checkString . parseString

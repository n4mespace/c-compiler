module Compiler.Lexer.Parse where

import           Compiler.Errors               (pretty)
import           Compiler.Lexer.Items
import           Compiler.Types

import           System.IO
import           Text.ParserCombinators.Parsec (ParseError, parse)

parseFile :: FilePath -> IO (Program, StmtT)
parseFile filePath = do
  withFile filePath ReadMode $ \handle -> do
    program <- hGetContents handle
    putStrLn "\n{-# SOURCE PROGRAM #-}"
    putStrLn program
    case parseString program of
      (_, Left e)  -> pretty e >> fail "lexer error"
      (p, Right r) -> return (p, r)

parseString :: Program -> (Program, Either ParseError StmtT)
parseString p = (p, parse mainParser "" p)

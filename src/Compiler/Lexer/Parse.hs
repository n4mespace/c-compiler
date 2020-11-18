module Compiler.Lexer.Parse where

import           Compiler.Lexer.Items
import           Compiler.Types

import           System.IO
import           Text.ParserCombinators.Parsec (ParseError, parse)

parseFile :: FilePath -> IO StmtT
parseFile filePath = do
  withFile filePath ReadMode $ \handle -> do
    program <- hGetContents handle
    putStrLn "\n{-# SOURCE PROGRAM #-}"
    putStrLn program
    case parseString program of
      Left e  -> print e >> fail "lexer error"
      Right r -> return r

parseString :: String -> Either ParseError StmtT
parseString = parse mainParser ""

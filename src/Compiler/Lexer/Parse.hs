module Compiler.Lexer.Parse where

import           Compiler.Lexer.Items
import           Compiler.Types

import           Text.ParserCombinators.Parsec
import           System.IO

parseFile :: FilePath -> IO StmtT
parseFile filePath = do
  withFile filePath ReadMode $ \handle -> do
    program <- hGetContents handle
    parseString program

parseString :: String -> IO StmtT
parseString program = do
  putStrLn "\n{-# SOURCE PROGRAM #-}"
  putStrLn program
  case parse mainParser "" program of
    Left e  -> print e >> fail "lexer error"
    Right r -> return r

module Lib
  ( main
  , parseFile
  )
where

import           Compiler.Parser
import           Compiler.Syntax.Control (Stmt)
import           Control.Monad           (void)
import           System.IO
import           Text.Parsec.Prim        (parse)
import           Text.Pretty.Simple      (pPrint)

main :: IO ()
main = void $ parseFile "test/lab1.c"

parseFile :: String -> IO Stmt
parseFile filePath = do
  withFile filePath ReadMode (\handle -> do
    program <- hGetContents handle
    case parse whileParser filePath program of
      Left e  -> print e >> fail "parse error"
      Right r -> do
        putStrLn "{-# INPUT PROGRAM #-}"
        putStrLn program
        putStrLn "\n{-# GENERATED AST-TOKENS #-}"
        pPrint r
        return r)

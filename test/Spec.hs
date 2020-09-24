module Main where

import           Compiler.Syntax.Control (Stmt)
import           Lib                     (parseFile)

main :: IO Stmt
main = parseFile "test/lab1.c"


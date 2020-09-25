module Main where

import           Compiler.Syntax.Control (Stmt)
import           Lib                     (main)

main :: IO ()
main = Lib.main "test/lab1.c" "test/chunk.asm"


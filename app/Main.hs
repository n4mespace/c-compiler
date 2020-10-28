module Main where

import qualified Lib

main :: IO ()
main = Lib.compileFile "test/lab1.c" "test/chunk.asm"

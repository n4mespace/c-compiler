module Main where

import qualified Lib

main :: IO ()
main = Lib.main "test/lab1.c" "test/chunk.asm"

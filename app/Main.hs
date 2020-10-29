module Main where

import qualified Lib

main :: IO ()
main = Lib.compileFile "test.c" "chunk.asm"

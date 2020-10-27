module Main where

import Test.Hspec

import qualified Lib

main :: IO ()
main = hspec $ do
  describe "Testing compiler" $ do
    it "generate assembly code from source file" $
      Lib.compileFile "test/lab1.c" "test/chunk.asm"

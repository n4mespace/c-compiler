module Lib
  ( main
  )
where

import qualified Data.ByteString.Lazy as BL
import           System.IO

main :: IO ()
main = do
  withFile "test/lab1.c" ReadMode (\handle -> do
    contents <- BL.hGetContents handle
    BL.putStr contents)

module Compiler.Parser where

import qualified Compiler.Syntax                  as S
import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as BCh
import qualified Data.Attoparsec.ByteString.Lazy  as BL
import qualified Data.ByteString                  as BS
import           Data.ByteString.UTF8             as BSU
import           Data.Word


word8ToChar :: Word8 -> Char
word8ToChar = toEnum . fromEnum

parseFunction :: BL.Parser S.Function
parseFunction = do
  returnType <- BL.string S.int <|> BL.string S.char
  BCh.skipSpace

  funcName <- BL.takeWhile $ (/= S.openParenthesis) . word8ToChar
  BCh.skipWhile (== S.openParenthesis)
  params <- BL.takeWhile $ (/= S.closeParenthesis) . word8ToChar

  BCh.skipWhile (/= S.openBrace)
  body <- BL.takeWhile $ (/= S.closeBrace) . word8ToChar

  parsedBody <- parseFunctionBody $ BS.tail body

  return $ S.Function returnType funcName params parsedBody

parseFunctionBody :: ByteString -> BL.Parser S.FunctionBody
parseFunctionBody body = do
  
  return $ S.FunctionBody [S.Lit 3]

try :: IO ()
try = do
  print $ BL.parseOnly parseFunction (BSU.fromString "int main(2) { return 2; }")





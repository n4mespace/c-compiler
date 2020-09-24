module Compiler.Parser

where

import           Compiler.Syntax.Arithmetic
import           Compiler.Syntax.Boolean
import           Compiler.Syntax.Control

import           Control.Monad
import           Data.Functor.Identity                  (Identity)
import           System.IO
import           Text.Parsec.Prim                       (ParsecT)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Char     (anyChar)
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Tok


-- Define C language
langDefC :: Tok.LanguageDef ()
langDefC = Tok.LanguageDef
  { Tok.commentStart    = "/*"
  , Tok.commentEnd      = "*/}"
  , Tok.commentLine     = "//"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter <|> char '_'
  , Tok.identLetter     = alphaNum <|> char '_'
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = reservedCNames
  , Tok.reservedOpNames = reservedCOpNames
  , Tok.caseSensitive   = True }

reservedCNames :: [String]
reservedCNames =
  [ "int", "char", "while", "for", "&&", "||", "!",
    "return", "if", "else", "True", "False", "void"]

reservedCOpNames :: [String]
reservedCOpNames =
  [ "+", "-", "*", "/", "=", "!=",
    "<", ">", "&&", "||", "!" ]

lexer :: Tok.GenTokenParser String () Identity
lexer = Tok.makeTokenParser langDefC

identifier :: ParsecT String () Identity String
identifier = Tok.identifier lexer -- parses an identifier

reserved :: String -> ParsecT String () Identity ()
reserved = Tok.reserved lexer -- parses a reserved name

reservedOp :: String -> ParsecT String () Identity ()
reservedOp = Tok.reservedOp lexer -- parses an operator

parens :: ParsecT String () Identity a -> ParsecT String () Identity a
parens = Tok.parens lexer -- parses surrounding parenthesis

braces :: ParsecT String () Identity a -> ParsecT String () Identity a
braces = Tok.braces lexer -- parses surrounding braces

integer :: ParsecT String () Identity Integer
integer = Tok.integer lexer -- parses an integer

semi :: ParsecT String () Identity String
semi = Tok.semi lexer -- parses a semicolon

whiteSpace :: ParsecT String () Identity ()
whiteSpace = Tok.whiteSpace lexer -- parses whitespace

symbol :: String -> ParsecT String () Identity String
symbol = Tok.symbol lexer -- parses s string

commaSep :: ParsecT String () Identity a -> ParsecT String () Identity [a]
commaSep = Tok.commaSep lexer -- parses 0 or more separated by comma values

-- "0b1011" -> 11
binaryInteger :: ParsecT String () Identity Integer
binaryInteger = do
  symbol "0b"
  numPart <- many1 $ oneOf "01"
  return $ binToDec (read numPart :: Integer)
    where
      binToDec :: Integral a => a -> a
      binToDec 0 = 0
      binToDec i = 2 * binToDec (div i 10) + mod i 10

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement <* eof

statement :: Parser Stmt
statement =   braces statement
          <|> parens statement
          <|> sequenceOfStmt

sequenceOfStmt :: Parser Stmt
sequenceOfStmt = do
  list <- many statement'
  return $ case length list of
    0 -> error "Empty file"
    _ -> Block list

statement' :: Parser Stmt
statement'
   =  try funcStmt
  <|> try returnStmt
  <|> try ifStmt
  <|> try whileStmt
  <|> try assignStmt
  <|> try simpleExpr

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond <- parens bExpression
  stmt1 <- braces statement
  reserved "else"
  stmt2 <- braces statement
  return $ If cond stmt1 stmt2

simpleExpr :: Parser Stmt
simpleExpr = ((Expr . ArExpr <$> aExpression)
          <|> (Expr . BoolExpr <$> bExpression)
        ) <*  semi

returnStmt :: Parser Stmt
returnStmt = do
  reserved "return"
  expr <- simpleExpr <|> (return ReturnNull <* semi)
  return $ Return expr

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- bExpression
  stmt <- braces statement
  return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt = do
  typeOfVar <- typeOfExpr
  varName <- identifier
  reservedOp "="
  expr <- aExpression <* semi
  return $ Assign typeOfVar varName expr

typeOfExpr :: Parser Type
typeOfExpr = (symbol "int" >> return INT)
         <|> (symbol "char" >> return CHAR)
         <|> (symbol "void" >> return VOID)

funcParam :: Parser FParams
funcParam = do
  typeOfP <- typeOfExpr
  name <- identifier
  return $ Param typeOfP name

funcStmt :: Parser Stmt
funcStmt = do
  typeOfF <- typeOfExpr
  name <- identifier
  params <- parens (commaSep funcParam) <|> return []
  body <- braces statement
  return $ Func typeOfF name params body

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators :: [[Operator Char () AExpr]]
aOperators = [ [Prefix (reservedOp "-" >> return  Neg              )          ]
             , [Infix  (reservedOp "*" >> return (ABinary Multiply)) AssocLeft,
                Infix  (reservedOp "/" >> return (ABinary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+" >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-" >> return (ABinary Subtract)) AssocLeft] ]

bOperators :: [[Operator Char () BExpr]]
bOperators = [ [Prefix (reservedOp "!"  >> return  Not        )           ]
             , [Infix  (reservedOp "&&" >> return (BBinary And)) AssocLeft,
                Infix  (reservedOp "||" >> return (BBinary Or )) AssocLeft] ]

singleChar :: Parser Char
singleChar = between (symbol "\'") (symbol "\'") anyChar

aTerm :: ParsecT String () Identity AExpr
aTerm =  parens aExpression
     <|> fmap Var identifier
     <|> (lookAhead (symbol "0b") >> fmap IntConst binaryInteger)
     <|> fmap IntConst integer
     <|> fmap CharConst singleChar

bTerm :: ParsecT String () Identity BExpr
bTerm =  parens bExpression
     <|> rExpression
     <|> (reserved "True"  >> return (BoolConst True ))
     <|> (reserved "False" >> return (BoolConst False))

rExpression :: ParsecT String () Identity BExpr
rExpression = do
  a1 <- aExpression
  op <- relation
  a2 <- aExpression
  return $ RBinary op a1 a2

relation :: ParsecT String () Identity RBinOp
relation =  (reservedOp ">" >> return Greater)
        <|> (reservedOp "<" >> return Less)

parseFile :: String -> IO Stmt
parseFile filePath = do
  withFile filePath ReadMode (\handle -> do
    program <- hGetContents handle
    case parse whileParser filePath program of
      Left e  -> print e >> fail "parse error"
      Right r -> return r)

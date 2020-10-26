module Compiler.Parser where

import           Compiler.Syntax.Arithmetic
import           Compiler.Syntax.Boolean
import           Compiler.Syntax.Control

import           Data.Functor.Identity                  (Identity)
import           System.IO
import           Text.Parsec.Prim                       (ParsecT)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token    as Tok

parseFile :: String -> IO Stmt
parseFile filePath = do
  withFile filePath ReadMode (\handle -> do
    program <- hGetContents handle
    case parse whileParser filePath program of
      Left e  -> print e >> fail "lexer error"
      Right r -> return r)

-- | Define C language
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
  [ "int", "char", "bool", "while", "for", "&&", "||", "!",
    "return", "if", "else", "true", "false", "void" ]

reservedCOpNames :: [String]
reservedCOpNames =
  [ "+", "-", "*", "/", "=", "!=", "%",
    "<", ">", "&&", "||", "!", "~" ]

lexer :: Tok.GenTokenParser String () Identity
lexer = Tok.makeTokenParser langDefC

-- | parses an identifier
identifier :: ParsecT String () Identity String
identifier = Tok.identifier lexer 

-- | parses a reserved name
reserved :: String -> ParsecT String () Identity ()
reserved = Tok.reserved lexer 

-- | parses an operator
reservedOp :: String -> ParsecT String () Identity ()
reservedOp = Tok.reservedOp lexer

-- | parses surrounding parenthesis
parens :: ParsecT String () Identity a -> ParsecT String () Identity a
parens = Tok.parens lexer 

-- | parses surrounding braces
braces :: ParsecT String () Identity a -> ParsecT String () Identity a
braces = Tok.braces lexer

-- | parses an integer
integer :: ParsecT String () Identity Integer
integer = Tok.integer lexer

-- | parses a semicolon
semi :: ParsecT String () Identity String
semi = Tok.semi lexer

-- | parses whitespace
whiteSpace :: ParsecT String () Identity ()
whiteSpace = Tok.whiteSpace lexer

-- | parses s string
symbol :: String -> ParsecT String () Identity String
symbol = Tok.symbol lexer 

-- | parses 0 or more separated by comma values
commaSep :: ParsecT String () Identity a -> ParsecT String () Identity [a]
commaSep = Tok.commaSep lexer 

-- | parse binary integers "0b11" -> 3
binaryInteger :: ParsecT String () Identity Integer
binaryInteger = symbol "0b" >> do
  numPart <- many1 $ oneOf "01"
  return $ binToDec (read numPart :: Integer)
    where
      binToDec :: Integral a => a -> a
      binToDec 0 = 0
      binToDec i = 2 * binToDec (div i 10) + mod i 10

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement <* eof

statement :: Parser Stmt
statement = braces statement
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
  <|> try emptyAssignStmt
  <|> try assignValue
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
simpleExpr = try (Expr . ArExpr <$> aExpression <* semi)
         <|> try (Expr . BoolExpr <$> bExpression <* semi)
         <?> "boolean or arythmetic expr"

returnStmt :: Parser Stmt
returnStmt = do
  reserved "return"
  expr <- simpleExpr <|> (Null <$ semi)
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
  expr <- simpleExpr
  return $ Assign typeOfVar varName expr

emptyAssignStmt :: Parser Stmt
emptyAssignStmt = do
  typeOfVar <- typeOfExpr
  varName <- identifier
  _ <- semi
  return $ EmptyAssign typeOfVar varName

assignValue :: Parser Stmt
assignValue = do
  varName <- identifier
  reservedOp "="
  expr <- simpleExpr
  return $ ValueAssign varName expr

typeOfExpr :: Parser Type
typeOfExpr = (symbol "int" >> return INT)
         <|> (symbol "char" >> return CHAR)
         <|> (symbol "bool" >> return BOOL)
         <|> (symbol "void" >> return VOID)
         <?> "bad type"

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
aOperators = [ [Prefix (reservedOp "-" >> return  Neg              )          ,
                Prefix (reservedOp "~" >> return  Complement       )           ]
             , [Infix  (reservedOp "*" >> return (ABinary Multiply)) AssocLeft,
                Infix  (reservedOp "/" >> return (ABinary Divide  )) AssocLeft,
                Infix  (reservedOp "%" >> return (ABinary Mod     )) AssocLeft ]
             , [Infix  (reservedOp "+" >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-" >> return (ABinary Subtract)) AssocLeft ] ]

bOperators :: [[Operator Char () BExpr]]
bOperators = [ [Prefix (reservedOp "!"  >> return  Not        )           ]
             , [Infix  (reservedOp "&&" >> return (BBinary And)) AssocLeft,
                Infix  (reservedOp "||" >> return (BBinary Or )) AssocLeft] ]

singleChar :: Parser Char
singleChar = between (symbol "\'") (symbol "\'") anyChar

aTerm :: ParsecT String () Identity AExpr
aTerm =  parens aExpression
     <|> fmap AVar identifier
     <|> fmap IntConst binaryInteger
     <|> fmap IntConst integer
     <|> fmap (IntConst . fromIntegral . fromEnum) singleChar
     <?> "constant arythmetic term"

bTerm :: ParsecT String () Identity BExpr
bTerm =  parens bExpression
     <|> fmap BVar identifier
     <|> rExpression
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <?> "constant boolean term"

rExpression :: ParsecT String () Identity BExpr
rExpression = do
  a1 <- aExpression
  op <- relation
  a2 <- aExpression
  return $ RBinary op a1 a2

relation :: ParsecT String () Identity RBinOp
relation =  (reservedOp ">" >> return Greater)
        <|> (reservedOp "<" >> return Less)


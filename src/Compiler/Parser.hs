module Compiler.Parser where

import           Compiler.Syntax.Control
import           Compiler.Syntax.Expression

import           Data.Functor.Identity               (Identity)
import           System.IO
import           Text.Parsec.Prim                    (ParsecT)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Tok

parseFile :: String -> IO StmtT
parseFile filePath = do
  withFile
    filePath
    ReadMode
    (\handle -> do
       program <- hGetContents handle
       case parse whileParser filePath program of
         Left e  -> print e >> fail "lexer error"
         Right r -> return r)

-- | Define C language
langDefC :: Tok.LanguageDef ()
langDefC =
  Tok.LanguageDef
    { Tok.commentStart = "/*"
    , Tok.commentEnd = "*/}"
    , Tok.commentLine = "//"
    , Tok.nestedComments = True
    , Tok.identStart = letter <|> char '_'
    , Tok.identLetter = alphaNum <|> char '_'
    , Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.reservedNames = reservedCNames
    , Tok.reservedOpNames = reservedCOpNames
    , Tok.caseSensitive = True
    }

reservedCNames :: [String]
reservedCNames =
  [ "int"
  , "char"
  , "bool"
  , "while"
  , "for"
  , "&"
  , "|"
  , "!"
  , "return"
  , "if"
  , "else"
  , "true"
  , "false"
  , "void"
  ]

reservedCOpNames :: [String]
reservedCOpNames =
  ["+", "-", "*", "/", "=", "!=", "%", "<", ">", "==", "&", "|", "!", "~"]

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
binaryInteger =
  symbol "0b" >> do
    numPart <- many1 $ oneOf "01"
    return $ binToDec (read numPart :: Integer)
  where
    binToDec :: Integral a => a -> a
    binToDec 0 = 0
    binToDec i = 2 * binToDec (div i 10) + mod i 10

-- | Parse code inside EOFs
whileParser :: Parser StmtT
whileParser = whiteSpace >> statements <* eof

statements :: Parser StmtT
statements = do
  list <- many statement'
  return $
    case length list of
      0 -> error "Empty block of code"
      _ -> Block list

statement' :: Parser StmtT
statement' =
  try funcStmt <|> 
  try returnStmt <|> 
  try ifElseStmt <|> 
  try ifStmt <|>
  try assignStmt <|>
  try emptyAssignStmt <|>
  try assignValue <|>
  try simpleExpr <|>
  try blockOfStmts

blockOfStmts :: ParsecT String () Identity StmtT
blockOfStmts = braces statements

ifElseStmt :: Parser StmtT
ifElseStmt = do
  reserved "if"
  cond <- parens expression
  stmt1 <- blockOfStmts
  reserved "else"
  stmt2 <- blockOfStmts
  return $ IfElse (Expr cond) stmt1 stmt2

ifStmt :: Parser StmtT
ifStmt = do
  reserved "if"
  cond <- parens expression
  stmt <- blockOfStmts
  return $ If (Expr cond) stmt

simpleExpr :: Parser StmtT
simpleExpr = Expr <$> expression <* semi

returnStmt :: Parser StmtT
returnStmt = do
  reserved "return"
  expr <- simpleExpr <|> (Null <$ semi)
  return $ Return expr

assignStmt :: Parser StmtT
assignStmt = do
  typeOfVar <- typeOfExpr
  varName <- identifier
  reservedOp "="
  expr <- simpleExpr
  return $ Assign typeOfVar varName expr

emptyAssignStmt :: Parser StmtT
emptyAssignStmt = do
  typeOfVar <- typeOfExpr
  varName <- identifier <* semi
  return $ EmptyAssign typeOfVar varName

assignValue :: Parser StmtT
assignValue = do
  varName <- identifier
  reservedOp "="
  expr <- simpleExpr
  return $ ValueAssign varName expr

typeOfExpr :: Parser Type
typeOfExpr =
  (reserved "int" >> return INT_T) <|> 
  (reserved "char" >> return CHAR_T) <|>
  (reserved "bool" >> return BOOL_T) <?> "bad type"

funcParam :: Parser FParamsT
funcParam = do
  typeOfP <- typeOfExpr
  name <- identifier
  return $ Param typeOfP name

funcStmt :: Parser StmtT
funcStmt = do
  typeOfF <- typeOfExpr
  name <- identifier
  params <- parens (commaSep funcParam) <|> return []
  body <- blockOfStmts
  return $ Func typeOfF name params body

expression :: Parser ExprT
expression = buildExpressionParser operators terms

operators :: [[Operator Char () ExprT]]
operators =
  [ [ Prefix (reservedOp "-" >> return (Unary Neg))
    , Prefix (reservedOp "~" >> return (Unary Complement))
    , Prefix (reservedOp "!" >> return (Unary Not))
    , Infix (reservedOp "==" >> return (Binary Equal)) AssocLeft
    ]
  , [ Infix (reservedOp "&" >> return (Binary And)) AssocLeft
    , Infix (reservedOp "|" >> return (Binary Or)) AssocLeft
    , Infix (reservedOp "*" >> return (Binary Multiply)) AssocLeft
    -- , Infix (reservedOp "/" >> return (Binary Divide)) AssocLeft
    , Infix (reservedOp "%" >> return (Binary Mod)) AssocLeft
    ]
  , [ Infix (reservedOp "+" >> return (Binary Add)) AssocLeft
    , Infix (reservedOp "-" >> return (Binary Subtract)) AssocLeft
    , Infix (reservedOp ">" >> return (Binary Greater)) AssocLeft
    , Infix (reservedOp "<" >> return (Binary Less)) AssocLeft
    ]
  ]

-- | Parse single char inside single quots
singleChar :: Parser Char
singleChar = between (symbol "\'") (symbol "\'") anyChar

terms :: ParsecT String () Identity ExprT
terms =
  parens expression <|> 
  try (Var <$> identifier) <|>
  try (Const . INT <$> binaryInteger) <|>
  try (Const . INT <$> integer) <|>
  try (Const . INT . fromIntegral . fromEnum <$> singleChar) <|>
  try (reserved "true" >> return (Const . BOOL $ True)) <|>
  try (reserved "false" >> return (Const . BOOL $ False)) <?> "constant arythmetic term"

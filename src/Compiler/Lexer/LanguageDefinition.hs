module Compiler.Lexer.LanguageDefinition where

import           Compiler.Syntax.Expression
import           Compiler.Types

import           Data.Functor.Identity               (Identity)
import           Text.Parsec.Prim                    (ParsecT)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Tok

-- C language definition
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
  , "return"
  , "if"
  , "else"
  , "true"
  , "false"
  , "void"
  ]

reservedCOpNames :: [String]
reservedCOpNames =
  [ "+"
  , "-"
  , "*"
  , "/"
  , "%"
  , "="
  , "%="
  , "+="
  , "-="
  , "*="
  , "/="
  , "!="
  , "<"
  , ">"
  , "=="
  , "&"
  , "|"
  , "!"
  , "~"
  ]

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

-- | Parse single char inside single quots
singleChar :: Parser Char
singleChar = between (symbol "\'") (symbol "\'") anyChar

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

-- | Boolean true
booleanTrue :: ParsecT String () Identity (Expr a)
booleanTrue = do
  reserved "true"
  return $ Const . BOOL $ True

-- | Boolean false
booleanFalse :: ParsecT String () Identity (Expr a)
booleanFalse = do
  reserved "false"
  return $ Const . BOOL $ False

-- | Boolean false or true
constBool :: ParsecT String () Identity (Expr a)
constBool =
  try booleanTrue <|>
  try booleanFalse

-- | Bin and dec int values (also char converted to ascii)
constInt :: ParsecT String () Identity (Expr a)
constInt =
  try (Const . INT <$> binaryInteger) <|>
  try (Const . INT <$> integer) <|>
  try (Const . INT . fromIntegral . fromEnum <$> singleChar)

-- | Identifier name
var :: ParsecT String () Identity (Expr a)
var = try $ Var <$> identifier

-- | Operator table
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
    , Infix (reservedOp "/" >> return (Binary Divide)) AssocLeft
    , Infix (reservedOp "%" >> return (Binary Mod)) AssocLeft
    ]
  , [ Infix (reservedOp "+" >> return (Binary Add)) AssocLeft
    , Infix (reservedOp "-" >> return (Binary Subtract)) AssocLeft
    , Infix (reservedOp ">" >> return (Binary Greater)) AssocLeft
    , Infix (reservedOp "<" >> return (Binary Less)) AssocLeft
    ]
  ]

-- | Terms parser
terms :: ParsecT String () Identity ExprT
terms =
  parens expression <|>
  var <|>
  constInt <|>
  constBool <?> "constant arythmetic term"

-- | Match operators and terms into expression
expression :: Parser ExprT
expression = buildExpressionParser operators terms

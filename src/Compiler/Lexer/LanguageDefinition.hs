module Compiler.Lexer.LanguageDefinition where

import           Compiler.Syntax.Expression
import           Compiler.Syntax.Statement
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
  , "break"
  , "continue"
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
booleanTrue :: ParsecT String () Identity ExprT
booleanTrue = do
  reserved "true"
  return $ Const . BOOL $ True

-- | Boolean false
booleanFalse :: ParsecT String () Identity ExprT
booleanFalse = do
  reserved "false"
  return $ Const . BOOL $ False

-- | Boolean false or true
constBool :: ParsecT String () Identity ExprT
constBool = booleanTrue <|> booleanFalse

-- | Bin and dec int values (also char converted to ascii)
constInt :: ParsecT String () Identity ExprT
constInt = choice
  [ try (Const . INT <$> binaryInteger)
  , try (Const . INT <$> integer)
  , try (Const . INT . fromIntegral . fromEnum <$> singleChar)
  ] <?> "Constant int (dec or bin) | char"

-- | Identifier name
var :: ParsecT String () Identity ExprT
var = Var <$> identifier

-- | Function call
callFunc :: ParsecT String () Identity ExprT
callFunc = do
  name <- identifier
  args <- parens $ commaSep funcArg
  return $ CallFunc name args

-- | Lift expression to stmt
exprStmt :: Parser StmtT
exprStmt = Expr <$> expression

-- | Expression with ; at the end
simpleExpr :: Parser StmtT
simpleExpr = exprStmt <* semi

-- | Function arguments
funcArg :: Parser FArgT
funcArg = FArg <$> expression

-- | Operator table
operators :: [[Operator Char () ExprT]]
operators =
  [ [ Prefix (reservedOp "-" >> return (Unary Neg))
    , Prefix (reservedOp "~" >> return (Unary Complement))
    , Prefix (reservedOp "!" >> return (Unary Not))
    ]
  , [ Infix (reservedOp "&" >> return (Binary And)) AssocLeft
    , Infix (reservedOp "|" >> return (Binary Or)) AssocLeft
    , Infix (reservedOp "*" >> return (Binary Multiply)) AssocLeft
    , Infix (reservedOp "/" >> return (Binary Divide)) AssocLeft
    , Infix (reservedOp "%" >> return (Binary Mod)) AssocLeft
    ]
  , [ Infix (reservedOp "+" >> return (Binary Add)) AssocLeft
    , Infix (reservedOp "-" >> return (Binary Subtract)) AssocLeft
    ]
  , [ Infix (reservedOp ">" >> return (Binary Greater)) AssocLeft
    , Infix (reservedOp "<" >> return (Binary Less)) AssocLeft
    , Infix (reservedOp "==" >> return (Binary Equal)) AssocLeft
    ]
  ]

-- | Terms parser
terms :: ParsecT String () Identity ExprT
terms = choice
  [ parens expression
  , try callFunc
  , try var
  , try constInt
  , try constBool
  ] <?> "Constant arythmetic term"

-- | Match operators and terms into expression
expression :: Parser ExprT
expression = buildExpressionParser operators terms

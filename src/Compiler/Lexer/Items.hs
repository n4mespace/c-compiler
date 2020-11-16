module Compiler.Lexer.Items where

import           Compiler.Lexer.LanguageDefinition
import           Compiler.Syntax.Control
import           Compiler.Syntax.Expression        (BinOp (..))
import           Compiler.Types

import           Data.Functor.Identity             (Identity)
import           Data.List                         (elemIndex)
import           Data.Maybe                        (fromJust)
import           Text.Parsec.Prim                  (ParsecT)
import           Text.ParserCombinators.Parsec

-- | Parse code inside EOFs
mainParser :: Parser StmtT
mainParser = whiteSpace >> statements stmtsInBlock <* eof

statements :: Parser StmtT -> Parser StmtT
statements stmtList = do
  list <- many stmtList
  return $
    case length list of
      0 -> error "Empty block of code"
      _ -> Block list

stmtsInBlock :: Parser StmtT
stmtsInBlock = choice
  [ try returnStmt
  , try ifElseStmt
  , try ifStmt
  , try assignmentStmt
  , try simpleExpr
  , try loopStmt
  , try funcStmt
  , try blockStmts
  ]

blockStmts :: ParsecT String () Identity StmtT
blockStmts = braces $ statements stmtsInBlock

stmtsInBody :: Parser StmtT
stmtsInBody = choice
  [ try returnStmt
  , try ifElseStmt
  , try ifStmt
  , try assignmentStmt
  , try simpleExpr
  , try loopStmt
  , try continueStmt
  , try breakStmt
  , try bodyStmts
  ]

bodyStmts :: ParsecT String () Identity StmtT
bodyStmts = braces $ statements stmtsInBody

nullStmt :: Parser StmtT
nullStmt = Null <$ semi

breakStmt :: Parser StmtT
breakStmt = do
  reserved "break"
  Break <$ semi

continueStmt :: Parser StmtT
continueStmt = do
  reserved "continue"
  Continue <$ semi

ifElseStmt :: Parser StmtT
ifElseStmt = do
  reserved "if"
  cond <- parens expression
  stmt1 <- bodyStmts
  reserved "else"
  stmt2 <- bodyStmts
  return $ IfElse cond stmt1 stmt2

ifStmt :: Parser StmtT
ifStmt = do
  reserved "if"
  cond <- parens expression
  stmt <- bodyStmts
  return $ If cond stmt

returnStmt :: Parser StmtT
returnStmt = do
  reserved "return"
  expr <- simpleExpr <|> nullStmt
  return $ Return expr

assignmentStmt :: Parser StmtT
assignmentStmt = choice
  [ try assignStmt
  , try opAssignStmt
  , try assignValueStmt
  , try emptyAssignStmt
  ] <?> "Assingment"

assignStmt :: Parser StmtT
assignStmt = do
  typeOfVar <- typeOfExpr
  varName <- identifier
  reservedOp "="
  expr <- expression
  _ <- semi
  return $ Assignment $ Assign typeOfVar varName expr

emptyAssignStmt :: Parser StmtT
emptyAssignStmt = do
  typeOfVar <- typeOfExpr
  varName <- identifier
  _ <- semi
  return $ Assignment $ EmptyAssign typeOfVar varName

assignValueStmt :: Parser StmtT
assignValueStmt = do
  varName <- identifier
  reservedOp "="
  expr <- expression
  _ <- semi
  return $ Assignment $ ValueAssign varName expr

opAssignStmt :: Parser StmtT
opAssignStmt = do
  varName <- identifier
  op <- typeOfAssignOp
  expr <- expression
  _ <- semi
  return $ Assignment $ OpAssign op varName expr

typeOfAssignOp :: Parser BinOp
typeOfAssignOp = choice
  [ reservedOp "%=" >> return Mod
  , reservedOp "+=" >> return Add
  , reservedOp "-=" >> return Subtract
  , reservedOp "*=" >> return Multiply
  , reservedOp "/=" >> return Divide
  ] <?> "Assign operator"

typeOfExpr :: Parser Type
typeOfExpr = choice
  [ reserved "int" >> return INT_T
  , reserved "char" >> return CHAR_T
  , reserved "bool" >> return BOOL_T
  ] <?> "Bad type"

funcParam :: Parser FParamT
funcParam = do
  typeOfP <- typeOfExpr
  name <- identifier
  return $ FParam typeOfP name

funcStmt :: Parser StmtT
funcStmt = choice
  [ try declareFuncStmt
  , try defineFuncStmt
  ] <?> "Function declaration | definition"

declareFuncStmt :: Parser StmtT
declareFuncStmt = do
  typeOfFunc <- typeOfExpr
  name <- identifier
  params <- parens $ commaSep funcParam
  _ <- semi
  return $ Func $ DeclareFunc typeOfFunc name params

defineFuncStmt :: Parser StmtT
defineFuncStmt = do
  typeOfFunc <- typeOfExpr
  name <- identifier
  params <- parens $ commaSep funcParam
  body <- bodyStmts
  return $ Func $ DefineFunc typeOfFunc name params body

loopStmt :: Parser StmtT
loopStmt = whileLoop <|>
           forLoop <?> "For | while loop"

whileLoop :: Parser StmtT
whileLoop = do
  reserved "while"
  cond <- parens expression
  body <- bodyStmts
  return $ Loop $ While cond body

forLoop :: Parser StmtT
forLoop = do
  reserved "for"
  header <- parens forLoopHeader
  body <- bodyStmts
  return $ Loop $ For header body

forLoopHeader :: Parser ForHeaderT
forLoopHeader = do
  initClause <- try assignmentStmt <|> nullStmt
  condClause <- try simpleExpr <|> nullStmt
  postClause <- try assignmentWithoutSemi <|> return Null
  return $ ForHeader initClause condClause postClause

assignmentWithoutSemi :: Parser StmtT
assignmentWithoutSemi = do
  input <- getInput
  let parensEndIdx = fromJust $ elemIndex ')' input
  setInput $ uncurry ((++) . (++ ";"))
           $ splitAt parensEndIdx input
  choice
    [ try opAssignStmt
    , try assignValueStmt
    ]

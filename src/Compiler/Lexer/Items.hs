module Compiler.Lexer.Items where

import           Compiler.Lexer.LanguageDefinition
import           Compiler.Syntax.Control
import           Compiler.Types

import           Data.Functor.Identity             (Identity)
import           Text.Parsec.Prim                  (ParsecT)
import           Text.ParserCombinators.Parsec

-- | Parse code inside EOFs
mainParser :: Parser StmtT
mainParser = whiteSpace >> statements <* eof

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
  try assignmentStmt <|>
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

assignmentStmt :: Parser StmtT
assignmentStmt =
  try assignStmt <|>
  try emptyAssignStmt <|>
  try assignValue <?> "Assingment"

assignStmt :: Parser StmtT
assignStmt = do
  typeOfVar <- typeOfExpr
  varName <- identifier
  reservedOp "="
  expr <- simpleExpr
  return $ Assignment $ Assign typeOfVar varName expr

emptyAssignStmt :: Parser StmtT
emptyAssignStmt = do
  typeOfVar <- typeOfExpr
  varName <- identifier <* semi
  return $ Assignment $ EmptyAssign typeOfVar varName

assignValue :: Parser StmtT
assignValue = do
  varName <- identifier
  reservedOp "="
  expr <- simpleExpr
  return $ Assignment $ ValueAssign varName expr

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

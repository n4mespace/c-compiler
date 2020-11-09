module Compiler.Lexer.Items where

import           Compiler.Lexer.LanguageDefinition
import           Compiler.Syntax.Control
import           Compiler.Syntax.Expression        (BinOp (..))
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
  try returnStmt <|>
  try ifElseStmt <|>
  try ifStmt <|>
  try assignmentStmt <|>
  try simpleExpr <|>
  try funcStmt <|>
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

exprStmt :: Parser StmtT
exprStmt = Expr <$> expression

simpleExpr :: Parser StmtT
simpleExpr =
  try (exprStmt <* semi) <|>
  try (callFuncStmt <* semi) <?> "Expression"

returnStmt :: Parser StmtT
returnStmt = do
  reserved "return"
  expr <- simpleExpr <|> (Null <$ semi)
  return $ Return expr

assignmentStmt :: Parser StmtT
assignmentStmt =
  try assignStmt <|>
  try emptyAssignStmt <|>
  try assignValueStmt <|>
  try opAssignStmt <?> "Assingment"

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
  varName <- identifier 
  _ <- semi
  return $ Assignment $ EmptyAssign typeOfVar varName

assignValueStmt :: Parser StmtT
assignValueStmt = do
  varName <- identifier
  reservedOp "="
  expr <- simpleExpr
  return $ Assignment $ ValueAssign varName expr

opAssignStmt :: Parser StmtT
opAssignStmt = do
  varName <- identifier
  op <- typeOfAssignOp
  expr <- simpleExpr
  return $ Assignment $ OpAssign op varName expr

typeOfAssignOp :: Parser BinOp
typeOfAssignOp =
  (reservedOp "%=" >> return Mod) <|>
  (reservedOp "+=" >> return Add) <|>
  (reservedOp "-=" >> return Subtract) <|>
  (reservedOp "*=" >> return Multiply) <|>
  (reservedOp "/=" >> return Divide) <?> "Assign operator"

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

funcArg :: Parser FArgsT
funcArg = do
  arg <- exprStmt <|> callFuncStmt
  return $ Arg arg

funcStmt :: Parser StmtT
funcStmt =
  try defineFuncStmt <|>
  try declareFuncStmt <?> "Function declaration | definition"

callFuncStmt :: Parser StmtT
callFuncStmt = do
  name <- identifier
  args <- parens $ commaSep funcArg
  return $ Func $ CallFunc name args

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
  body <- blockOfStmts
  return $ Func $ DefineFunc typeOfFunc name params body

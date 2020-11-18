module Compiler.Errors where

import           Compiler.Syntax.Error
import           Compiler.Syntax.Statement (Name)
import           Compiler.Types

import           Data.List                 (intercalate)
import           Data.Map.Strict           as M

-- List of common errors
undefinedObjectsErr :: EnvMap -> Either ErrT a
undefinedObjectsErr undefEnvMap = Left $ BadExpression $
  "Found objects without being defined (initialized): " <>
    intercalate ", " (snd <$> M.keys undefEnvMap)

emptyBlockErr :: Either ErrT a
emptyBlockErr = Left EmptyBlock

lexerErr :: Either ErrT a
lexerErr = Left LexerError

breakOutsideTheLoopErr :: Either ErrT a
breakOutsideTheLoopErr = Left BreakOutsideTheLoop

continueOutsideTheLoopErr :: Either ErrT a
continueOutsideTheLoopErr = Left ContinueOutsideTheLoop

-- Identifiers errors
unknownVarErr :: Name -> Either ErrT a
unknownVarErr = Left . BadExpression . ("Unknown var: " <>)

uninitVarErr :: Name -> Either ErrT a
uninitVarErr = Left . BadExpression . ("Uninitialized var: " <>)

alreadyDeclaredVarErr :: Name -> Either ErrT a
alreadyDeclaredVarErr =
  Left . BadExpression . ("Already declared var: " <>)

-- Function errors
unknownFuncErr :: Name -> Either ErrT a
unknownFuncErr = Left . BadExpression . ("Unknown function: " <>)

mainFuncNotFoundErr :: Either ErrT a
mainFuncNotFoundErr = Left $ BadExpression "Cannot find main function"

wrongNumArgsErr :: Name -> [FParamT] -> Either ErrT a
wrongNumArgsErr fName fParams =
  Left $ BadExpression $
    "Wrong number of arguments in function: " <> fName <>
    ". Must be: " <> show (length fParams) <> " args"

differentParamsErr :: Name -> Either ErrT a
differentParamsErr = Left . BadExpression . (
  "Different parameters in declaration and definition in function: " <>)

alreadyDefinedFuncErr :: Name -> Either ErrT a
alreadyDefinedFuncErr =
  Left . BadExpression . ("Multiple function definition: " <>)

alreadyDeclaredFuncErr :: Name -> Either ErrT a
alreadyDeclaredFuncErr =
  Left . BadExpression . ("Multiple function declaration: " <>)

-- Loop errors
initClauseErr :: Either ErrT a
initClauseErr = Left $ BadExpression "Invalid initialize clause"

condClauseErr :: Either ErrT a
condClauseErr = Left $ BadExpression "Invalid condition clause"

postClauseErr :: Either ErrT a
postClauseErr = Left $ BadExpression "Invalid post clause"

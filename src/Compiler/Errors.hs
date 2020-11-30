module Compiler.Errors where

import           Compiler.Syntax.Error
import           Compiler.Syntax.Statement
import           Compiler.Types

import           Data.List                 as L
import           Data.Map.Strict           as M
import           Text.Pretty.Simple        (CheckColorTty (CheckColorTty),
                                            OutputOptions (..),
                                            defaultOutputOptionsDarkBg,
                                            pPrintOpt)

-- List of common errors
undefinedObjectsErr :: Program -> EnvMap -> Either ErrT a
undefinedObjectsErr p undef =
  let
    undefObj :: Name
    undefObj = snd $ head $ M.keys undef
  in Left $ BadExpression $
    findErrLine p undefObj <>
    "Found function without being defined: " <>
    undefObj

emptyBlockErr :: Either ErrT a
emptyBlockErr = Left EmptyBlock

lexerErr :: Either ErrT a
lexerErr = Left LexerError

breakOutsideTheLoopErr :: Program -> String -> Either ErrT String
breakOutsideTheLoopErr p asm =
  if "__break" `inString` asm
    then Left $
      BreakOutsideTheLoop $
        findErrLine p "break" <> "Break not in loop body"
    else return asm

continueOutsideTheLoopErr :: Program -> String -> Either ErrT String
continueOutsideTheLoopErr p asm =
  if "__continue" `inString` asm
    then Left $
      ContinueOutsideTheLoop $
        findErrLine p "continue" <> "Continue not in loop body"
    else return asm

inString :: String -> String -> Bool
inString search str =
  case L.isPrefixOf search `L.findIndex` L.tails str of
    Just _  -> True
    Nothing -> False

-- Identifiers errors
unknownVarErr :: Program -> Name -> Either ErrT a
unknownVarErr p vName = Left $
  BadExpression $
    findErrLine p vName <> "Unknown var: " <> vName

uninitVarErr :: Program -> Name -> Either ErrT a
uninitVarErr p vName = Left $
  BadExpression $
    findErrLine p vName <> "Uninitialized var: " <> vName

alreadyDeclaredVarErr :: Program -> Name -> Either ErrT a
alreadyDeclaredVarErr p vName = Left $
  BadExpression $
    findErrLine p vName <> "Already declared var: " <> vName

-- Function errors
unknownFuncErr :: Program -> Name -> Either ErrT a
unknownFuncErr p fName = Left $
  BadExpression $
    findErrLine p fName <> "Unknown function: " <> fName

mainFuncNotFoundErr :: Either ErrT a
mainFuncNotFoundErr = Left $ BadExpression "Cannot find main function"

wrongNumArgsErr :: Program -> Name -> [FParamT] -> Either ErrT a
wrongNumArgsErr p fName fParams =
  Left $ BadExpression $
    findErrLine p fName <>
    "Wrong number of arguments in function call: " <> fName <>
    ". Must be: " <> show (length fParams) <> " args"

differentParamsErr :: Program -> Name -> Either ErrT a
differentParamsErr p fName = Left $
  BadExpression $
    findErrLine p fName <>
    "Different parameters in declaration " <>
    "and definition in function: " <> fName

alreadyDefinedFuncErr :: Program -> Name -> Either ErrT a
alreadyDefinedFuncErr p fName = Left $
  BadExpression $
   findErrLine p fName <>
   "Multiple function definition: " <> fName

alreadyDeclaredFuncErr :: Program -> Name -> Either ErrT a
alreadyDeclaredFuncErr p fName = Left $
  BadExpression $
    findErrLine p fName <>
    "Multiple function declaration: " <> fName

-- Loop errors
initClauseErr :: Either ErrT a
initClauseErr = Left $ BadExpression "Invalid initialize clause"

condClauseErr :: Either ErrT a
condClauseErr = Left $ BadExpression "Invalid condition clause"

postClauseErr :: Either ErrT a
postClauseErr = Left $ BadExpression "Invalid post clause"

findErrLine :: Program -> String -> String
findErrLine program = ((reverse . lines $ program) `findErrInProgramLines`)

findErrInProgramLines :: [String] -> String -> String
findErrInProgramLines programLines errId = go programLines numLines
  where
    numLines :: Int
    numLines = length programLines

    spaceLen :: String -> Int
    spaceLen = length . takeWhile (== ' ')

    charLen :: String -> Int
    charLen = length . dropWhile (== ' ')

    go :: [String] -> Int -> String
    go [] _ = ""
    go (x:xs) n =
      case L.isPrefixOf errId `L.findIndex` words x of
        Nothing -> go xs $ n - 1
        Just _  -> unlines
          [ "\nline " <> show n <> "| " <> x
          , replicate (8 + spaceLen x) ' ' <> replicate (charLen x) '^'
          ]

pretty :: Show a => a -> IO ()
pretty = pPrintOpt CheckColorTty opts
  where
    opts :: OutputOptions
    opts = defaultOutputOptionsDarkBg
      { outputOptionsIndentAmount = 4
      , outputOptionsPageWidth = 65
      , outputOptionsCompact = True
      }

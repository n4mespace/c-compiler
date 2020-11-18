module Compiler.Syntax.Error where


data Err a
  = UnExpectedType a
  | BadReturn
  | EmptyBlock
  | CannotAssignTo String a
  | TypesMissmatch a a
  | BadExpression String
  | ReservedName String
  | LexerError
  | Error
  | ContinueOutsideTheLoop
  | BreakOutsideTheLoop
  deriving (Show, Read, Eq)

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
  | ContinueOutsideTheLoop String
  | BreakOutsideTheLoop String
  deriving (Show, Read, Eq)

module Compiler.Types where

import           Compiler.Syntax.Control
import           Compiler.Syntax.Error          (Err)
import           Compiler.Syntax.Expression

import           Control.Monad.Trans.State.Lazy (StateT)
import           Data.Map.Strict                (Map)

-- Awailable types
data Type
  = INT_T
  | CHAR_T
  | BOOL_T
  deriving (Show, Eq)

-- | Typed syntax construction
type StmtT = Stmt Type

type ExprT = Expr Type

type FParamsT = FParams Type

type FArgsT = FArgs Type

type FuncT = Func Type

type AssignmentT = Assignment Type

type ErrT = Err Type

-- | Counter for variable address generation
type EbpOffset = Int

-- | Every block of code adds 1 to scope counter
type Scope = Int

-- | Counter for scope depth
type CurrScope = Int

-- | Map for identifiers
type EnvMap = Map (Scope, Name) EbpOffset

-- | Global state map
type GlobalEnv = (CurrScope, EnvMap)

-- | Global state for grammar check
type GlobalState = StateT GlobalEnv (Either ErrT) StmtT

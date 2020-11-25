module Compiler.Types where

import           Compiler.Syntax.Error
import           Compiler.Syntax.Expression
import           Compiler.Syntax.Statement

import           Control.Monad.Trans.State.Lazy (StateT)
import           Data.Map.Strict                (Map)

-- Awailable types
data Type
  = INT_T
  | CHAR_T
  | BOOL_T
  deriving (Show, Eq, Read)

-- | Typed syntax construction
type StmtT = Stmt Type

type ExprT = Expr Type

type FParamT = FParam Type

type FArgT = FArg Type

type FuncT = Func Type

type ForHeaderT = ForHeader Type

type LoopT = Loop Type

type AssignmentT = Assignment Type

type ErrT = Err Type

-- | Counter for variable address generation
type EbpOffset = Int

-- | Every block of code adds 1 to scope counter
type Scope = Int

-- | Counter for scope depth
type CurrScope = Int

-- | Name of function scope
type FuncName = String

-- | Name of current function scope
type CurrFuncName = String

-- | Whether expr is defined
type Defined = Bool

-- | Key to EnvMap
type ScopedName = (Scope, Name)

-- | Value to EnvMap
type Env = (EbpOffset, Defined, FuncName, [FParamT])

-- | Global map for extra info
type EnvMap = Map ScopedName Env

-- | Program string
type Program = String

-- | Global state map
type GlobalEnv = (CurrScope, CurrFuncName, EnvMap, Program)

-- | Global state for grammar check
type GlobalState a = StateT GlobalEnv (Either ErrT) a

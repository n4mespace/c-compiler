{-# LANGUAGE FlexibleInstances #-}

module Compiler.Generator.MASM
  ( generateMASM
  ) where

import           Compiler.Grammar           (Err (..))
import           Compiler.Syntax.Control
import           Compiler.Syntax.Expression

import           Data.Functor               ((<$>))
import           Data.Monoid                ((<>))

generateMASM :: FilePath -> StmtT -> IO ()
generateMASM destination program = do
  case emit program of
    Right generatedASM -> do
      putStrLn "\n{-# GENERATED .ASM #-}"
      putStrLn generatedASM
      destination `writeFile` generatedASM
    Left e -> print e >> fail "asm gen error"

-- | Make program capable to generate asm code
class Emittable p where
  emit :: p -> Either Err String

instance Emittable StmtT where
  emit (Block stmts) = emitBlock $ emit <$> stmts
  emit (Func _ _ _ stmts) =
    emitBlock
      [ emitLn "push ebp"
      , emitNLn "mov ebp, esp"
      , emitNLn ""
      , emit stmts
      , emitNLn ""
      , emitNLn "mov esp, ebp"
      , emitNLn "pop ebp"
      , emitNLn ""
      , emitNLn "mov b, eax"
      ]
  emit (Assign _ name (Expr expr)) =
    emitBlock [emit expr, emitNLn $ "mov " <> name <> ", eax"]
  emit (ValueAssign name (Expr expr)) =
    emitBlock [emit expr, emitNLn $ "mov " <> name <> ", eax"]
  emit (EmptyAssign _ _) = Right ""
  emit (Return Null) = Right ""
  emit (Return (Expr expr)) = emit expr
  emit s = Left $ BadExpression $ "unknown statement: " <> show s

instance Emittable ExprT where
  emit (Var name) = emitNLn $ "mov eax, " <> name
  emit (Const c) = emitNLn "mov eax, " <$*> emit c
  emit (Unary op expr) =
    case op of
      Neg        -> emit expr <$*> emitNLn "neg eax"
      Not        -> emit expr <$*> emitNLn "neg eax"
      Complement -> emit expr <$*> emitNLn "xor eax, -1"
  emit (Binary op expr1 expr2) =
    case op of
      Subtract -> emitBinary sub
      Add      -> emitBinary add
      Mod      -> emitBinary modulo
      badOp    -> Left $ BadExpression $ "unknown operation: " <> show badOp
    where
      emitBinary :: Either Err String -> Either Err String
      emitBinary f = emitBlock [emit expr2, pushEax, emit expr1, f]

instance Emittable C where
  emit (INT i) = Right $ show i
  emit (CHAR c) = Right $ show c
  emit (BOOL b) =
    if b
      then Right "1"
      else Right "0"

-- Helpers
emitBlock :: [Either Err String] -> Either Err String
emitBlock = (concat <$>) . sequence

(<$*>) :: Either Err String -> Either Err String -> Either Err String
expr1 <$*> expr2 = (<>) <$> expr1 <*> expr2

infixr 6 <$*>

emitNLn :: String -> Either Err String
emitNLn = Right . ("\n\t" <>)

emitLn :: String -> Either Err String
emitLn = Right . ("\t" <>)

emitLabel :: String -> Either Err String
emitLabel = Right . (<> ":\n")

-- Basic math functions
popEbx :: Either Err String
popEbx = emitNLn "pop ebx"

pushEax :: Either Err String
pushEax = emitNLn "push eax"

sub :: Either Err String
sub = popEbx <$*> emitNLn "sub eax, ebx"

add :: Either Err String
add = popEbx <$*> emitNLn "add eax, ebx"

modulo :: Either Err String
modulo =
  popEbx <$*>
  emitNLn "xor edx, edx" <$*> emitNLn "div ebx" <$*> emitNLn "mov eax, edx"

module Compiler.Generator.MASM
  ( generateMASM
  ) where

import           Compiler.Syntax.Arithmetic
import           Compiler.Syntax.Boolean
import           Compiler.Syntax.Control

import           Compiler.Grammar           (Err (..))
import           Data.Functor               ((<$>))
import           Data.Monoid                ((<>))


generateMASM :: FilePath -> Stmt -> IO ()
generateMASM destination program = do
  case asm of
    Left e -> print e >> fail "asm gen error"
    Right generatedASM -> do
      putStrLn "\n{-# GENERATED .ASM #-}"
      putStrLn generatedASM
      destination `writeFile` generatedASM
  where
    asm :: Either Err String
    asm = codeSection program 

codeSection :: Stmt -> Either Err String
codeSection = flip (<$*>) (emitNLn "mov b, eax") . emitCode

-- Make program capable to generate code to different .asm sections
class Emittable e where
  emitCode :: e -> Either Err String

instance Emittable Stmt where
  emitCode (Block stmts)        = emitBlock $ emitCode <$> stmts
  emitCode (Func _ _ _ stmts)   = emitCode stmts
  emitCode (Assign t name stmt) = Left $ BadExpression $ "assign: " <> name
  emitCode (Return Null)  = emitLn "ret"
  emitCode (Return (Expr expr)) = emitCode expr
  emitCode _                    = Left $ BadExpression "unknown statement"

instance Emittable Expr where
  emitCode (ArExpr aExpr)   = emitCode aExpr
  emitCode (BoolExpr bExpr) = Left $ BadExpression "unknown expression"

instance Emittable AExpr where
  emitCode (Neg aExpr) = emitCode aExpr 
                    <$*> emitNLn "neg eax"
  emitCode (Var name) = Left $ BadExpression $ "var: " <> name
  emitCode (IntConst i) = emitNLn $ "mov eax, " <> show i
  emitCode (Complement aExpr) = emitCode aExpr 
                           <$*> emitNLn "xor eax, -1"
  emitCode (ABinary op aExpr1 aExpr2) =
    case op of
      Subtract -> emitBinaryFunc sub
      Add -> emitBinaryFunc add
      Mod -> emitBinaryFunc modulo
      badOp -> Left $ BadExpression $ "unknown operation: " <> show badOp
    where
      emitBinaryFunc :: Either Err String -> Either Err String
      emitBinaryFunc f = 
        emitBlock [ emitCode aExpr2
                  , pushEax
                  , emitCode aExpr1
                  , f ]

-- Helpers
emitBlock :: [Either Err String] -> Either Err String
emitBlock = (concat <$>) . sequence

(<$*>) :: Either Err String -> Either Err String -> Either Err String
expr1 <$*> expr2 = (<>) <$> expr1
                        <*> expr2
infixr 6 <$*>

emitLn :: String -> Either Err String
emitLn = Right . ("\t" <>) . (<> "\n")

emitNLn :: String -> Either Err String
emitNLn = Right . ("\n\t" <>)

emitLabel :: String -> Either Err String
emitLabel = Right . (<> ":\n")

-- Basic math functions
popEbx :: Either Err String
popEbx = emitNLn "pop ebx"

popEax :: Either Err String
popEax = emitNLn "pop eax"

pushEax :: Either Err String
pushEax = emitNLn "push eax"

sub :: Either Err String
sub = popEbx 
 <$*> emitNLn "sub eax, ebx"

add :: Either Err String
add = popEbx 
 <$*> emitNLn "add eax, ebx"

modulo :: Either Err String
modulo = popEbx 
    <$*> emitNLn "xor edx, edx"
    <$*> emitNLn "div ebx"
    <$*> emitNLn "mov eax, edx"

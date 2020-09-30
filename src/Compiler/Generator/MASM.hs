module Compiler.Generator.MASM
    ( generateMASM
    )
where

import           Compiler.Syntax.Arithmetic
import           Compiler.Syntax.Boolean
import           Compiler.Syntax.Control

import           Compiler.Grammar           (Err (..))
import           Data.Functor               ((<$>))
import           Data.Monoid                ((<>))


generateMASM :: Stmt -> IO (Either Err String)
generateMASM p = return $ codeSection p


codeSection :: Stmt -> Either Err String
codeSection program = case emitted of
  Left e -> Left e
  Right p -> Right $ p                
          <> emitNLn "mov b, eax"
  where
    emitted :: Either Err String
    emitted = emitCode program

-- Make program capable to generate code to different .asm sections
class Emittable e where
  emitCode :: e -> Either Err String


instance Emittable Stmt where
  emitCode (Block stmts) = foldl1 (<>) $ emitCode <$> stmts
  emitCode (Func _ _ _ stmts) = emitCode stmts
  emitCode (Assign t name stmt) = Left $ BadExpression $ "assign: " <> name
  emitCode (Return (Expr expr)) = emitCode expr
  emitCode ReturnNull = Right "ret"
  emitCode _ = Left $ BadExpression "unknown statement"

instance Emittable Expr where
  emitCode (ArExpr aExpr)   = emitCode aExpr
  emitCode (BoolExpr bExpr) = Left $ BadExpression "unknown expression"

instance Emittable AExpr where
  emitCode (Neg aExpr) = case emitCode aExpr of
        Left e -> Left e
        Right expr -> Right $ expr
                   <> emitNLn "neg eax"
  emitCode (Var name) = Left $ BadExpression $ "var: " <> name
  emitCode (IntConst i) = Right $ emitNLn $ "mov eax, " <> show i
  emitCode (Complement aExpr) = case emitCode aExpr of
        Left e -> Left e
        Right expr -> Right $ expr
                   <> emitNLn "xor eax, -1" 
  emitCode (ABinary op aExpr1 aExpr2) =
    case op of
      Subtract -> case emitCode aExpr2 of
        Left e -> Left e
        Right expr2 -> case emitCode aExpr1 of
          Left e -> Left e
          Right expr1 -> Right $ expr2
                      <> pushEax 
                      <> expr1
                      <> sub
      badOp -> Left $ BadExpression $ "unknown operation: " <> show badOp


-- Helpers
emitLn :: String -> String
emitLn = ("\t" <>) . (<> "\n")

emitNLn :: String -> String
emitNLn = ("\n\t" <>) 

emitLabel :: String -> String
emitLabel = (<> ":\n")

-- Basic math functions
popEbx :: String
popEbx = emitNLn "pop ebx"

popEax :: String
popEax = emitNLn "pop eax"

pushEax :: String
pushEax = emitNLn "push eax"

sub :: String
sub = popEbx 
   <> emitNLn "sub eax, ebx"

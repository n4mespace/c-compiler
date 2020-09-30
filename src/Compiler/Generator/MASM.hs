module Compiler.Generator.MASM
    ( generateMASM
    )
where

import           Compiler.Syntax.Arithmetic
import           Compiler.Syntax.Boolean
import           Compiler.Syntax.Control

import           Compiler.Grammar           (Err)
import           Data.Functor               ((<$>))
import           Data.Monoid                ((<>))


generateMASM :: Either Err Stmt -> IO String
generateMASM (Left e)  = print e >> fail "parse error"
generateMASM (Right p) = return $ codeSection p

codeSection :: Stmt -> String
codeSection program = case emittedProg of
  [] -> ""
  _  -> emittedProg <> emitNLn "mov b, eax"
  where
    emittedProg :: String
    emittedProg = emitCode program


-- Make program capable to generate code to different .asm sections
class Emittable e where
  emitCode :: e -> String


instance Emittable Stmt where
  emitCode (Block stmts) = concat $ emitCode <$> stmts
  emitCode (Func _ _ _ stmts) = emitLn . emitCode $ stmts
  emitCode (Assign t name stmt) = ""
  emitCode (Return (Expr expr)) = emitCode expr
  emitCode ReturnNull = "ret"
  emitCode _ = ""

instance Emittable Expr where
  emitCode (ArExpr aExpr)   = emitCode aExpr
  emitCode (BoolExpr bExpr) = ""

instance Emittable AExpr where
  emitCode (Neg aExpr) = emitCode aExpr <> emitNLn "neg eax"
  emitCode (Var _) = ""
  emitCode (IntConst i) = emitNLn $ "mov eax, " <> show i
  emitCode (Complement aExpr) = emitCode aExpr 
                             <> emitNLn "xor eax, -1" 
  emitCode (ABinary op aExpr1 aExpr2) =
    case op of
      Subtract -> emitCode aExpr2
               <> pushEax 
               <> emitCode aExpr1
               <> sub
      _ -> ""


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
sub = popEbx <> emitNLn "sub eax, ebx"

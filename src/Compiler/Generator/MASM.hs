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
  _  -> emittedProg <> "\tmov b, eax"
  where
    emittedProg :: String
    emittedProg = emitCode program


-- Make program capable to generate code to different .asm sections
class Emittable e where
  emitCode :: e -> String


instance Emittable Stmt where
  emitCode (Block stmts) = foldl1 (++) $ emitCode <$> stmts
  emitCode (Func t name params stmts) = emitLn . emitCode $ stmts
  emitCode (Assign t name stmt) = ""
  emitCode (Return (Expr (ArExpr (IntConst i))))  = "mov eax, " <> show i
  emitCode ReturnNull = "ret"
  emitCode _ = ""


instance Emittable Expr where
  emitCode (ArExpr aExpr)   = emitCode aExpr
  emitCode (BoolExpr bExpr) = ""

instance Emittable AExpr where
  emitCode (Neg aExpr) = ""
  emitCode (Var name) = name
  emitCode (IntConst i) = show i
  emitCode (ABinary op aExpr1 aExpr2) =
    case op of
      Add      -> "add " <> emitCode aExpr1 <> ", " <> emitCode aExpr2
      Subtract -> "sub " <> emitCode aExpr1 <> ", " <> emitCode aExpr2
      Multiply -> "mul " <> emitCode aExpr1 <> ", " <> emitCode aExpr2
      Divide   -> "div " <> emitCode aExpr1 <> ", " <> emitCode aExpr2


-- Helpers
emitLn :: String -> String
emitLn = ("\t" <>) . (<> "\n")

emitJne :: String -> String
emitJne = emitLn . ("jne " <>)

emitJmp :: String -> String
emitJmp = emitLn . ("jmp " <>)

emitLabel :: String -> String
emitLabel = (<> ":\n")

getLbl :: Integer -> (Integer, String)
getLbl count = (count + 1, "L" <> show count)


-- Basic math functions
popEbx :: String
popEbx = emitLn "pop ebx"

popEax :: String
popEax = emitLn "pop eax"

pushEax :: String
pushEax = emitLn "push eax"

add :: String
add = popEbx <> emitLn "add eax, ebx"

sub :: String
sub = popEbx <> emitLn "add eax, ebx" <> emitLn "neg eax"

mul :: String
mul = popEbx <> emitLn "mul ebx"

divide :: String
divide =
     emitLn "mov ebx, eax"
  <> popEax
  <> emitLn "mov edx, 0"
  <> emitLn "DIV ebx"

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


-- Make program capable to generate code to different .asm sections
class Emittable e where
  emitData :: e -> String
  emitConst :: e -> String
  emitCode :: e -> String


generateMASM :: Either Err Stmt -> IO String
generateMASM (Left e) = return "cannot generate .asm"
generateMASM (Right p) = return $
     imports
  <> constSection p
  <> dataSection p
  <> codeSection p


instance Emittable Stmt where

  emitData (Block stmts) = foldl1 (++) $ emitData <$> stmts
  emitData (Func t name params stmts) = emitData stmts
  emitData (Assign t name (Expr (ArExpr (IntConst i)))) = name <> " dword " <> show i
  emitData (Assign t name (Expr (ArExpr (CharConst i)))) = name <> " dword " <> show i
  emitData (Return stmt) = ""
  emitData ReturnNull = ""
  emitData _ = ""

  emitConst (Return stmt)              = ""
  emitConst ReturnNull                 = ""
  emitConst (Assign t name stmt)       = ""
  emitConst (Func t name params stmts) = emitConst stmts
  emitConst (Block stmts)              = foldl1 (++) $ emitConst <$> stmts
  emitConst _                          = ""

  emitCode (Block stmts) = foldl1 (++) $ emitCode <$> stmts
  emitCode (Func t name params stmts) =
       name <> " proc\n"
    <> (emitLn . emitCode $ stmts)
    <> name <> " endp\n\n"
  emitCode (Assign t name stmt) = ""
  emitCode (Return (Expr (ArExpr (IntConst i))))  = "mov eax, " <> show i
  emitCode (Return (Expr (ArExpr (CharConst i)))) = "mov eax, " <> show i
  emitCode ReturnNull = "ret"
  emitCode _ = ""


instance Emittable Expr where
  emitData (ArExpr aExpr)   = emitData aExpr
  emitData (BoolExpr bExpr) = ""

  emitConst (ArExpr aExpr)   = emitConst aExpr
  emitConst (BoolExpr bExpr) = ""

  emitCode (ArExpr aExpr)   = emitCode aExpr
  emitCode (BoolExpr bExpr) = ""

instance Emittable AExpr where
  emitData (Var name) = ""
  emitData (IntConst i) = show i
  emitData (CharConst i) = show i
  emitData (ABinary op aExpr1 aExpr2) = emitData aExpr1 <> "\n" <> emitData aExpr2
  emitData (Neg aExpr) = ""

  emitConst _ = ""

  emitCode (Neg aExpr) = ""
  emitCode (Var name) = name
  emitCode (IntConst i) = show i
  emitCode (CharConst i) = "\'" <> show i <> "\'"
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



imports :: String
imports =
     ".586\n"
  <> ".model flat, stdcall\n\n"
  <> "option casemap: none\n\n"
  -- <> "include \\masm32\\include\\kernel32.inc\n"
  -- <> "include \\masm32\\include\\user32.inc\n"
  -- <> "include \\masm32\\include\\windows.inc\n\n"
  -- <> "includelib \\masm32\\lib\\kernel32.lib\n"
  -- <> "includelib \\masm32\\lib\\user32.lib\n"

dataSection :: Stmt -> String
dataSection program = case emittedProg of
  [] -> ""
  _  -> "section .data\n" <> emittedProg <> "\n"
  where
    emittedProg = emitData program

constSection :: Stmt -> String
constSection program = case emittedProg of
  [] -> ""
  _  -> "section .const\n" <> emittedProg <> "\n"
  where
    emittedProg = emitConst program

codeSection :: Stmt -> String
codeSection program = case emittedProg of
  [] -> ""
  _  -> "section .code\n" <> emittedProg
  where
    emittedProg = emitCode program

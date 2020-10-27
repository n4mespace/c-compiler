{-# LANGUAGE FlexibleInstances #-}

module Compiler.Generator.MASM
  ( generateMASM
  ) where

import           Compiler.Grammar           (Err (..))
import           Compiler.Syntax.Control
import           Compiler.Syntax.Expression

import           Data.Functor               ((<$>))
import           Data.Monoid                ((<>))
import           System.Random              (newStdGen, randomRs, StdGen)
import           System.IO.Unsafe           (unsafePerformIO)

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
      , nLine
      , emit stmts
      , nLine
      , emitLbl "__ret"
      , emitNLn "mov esp, ebp"
      , emitNLn "pop ebp"
      , nLine
      , emitNLn "mov b, eax"
      ]
  emit (Assign _ name (Expr expr)) =
    emitBlock [emit expr, emitNLn $ "mov " <> name <> ", eax"]
  emit (ValueAssign name (Expr expr)) =
    emitBlock [emit expr, emitNLn $ "mov " <> name <> ", eax"]
  emit (EmptyAssign _ _) = Right ""
  emit (Return Null) = goToReturn
  emit (Return (Expr expr)) = emit expr 
                         <$*> goToReturn
  emit (If (Expr cond) stmt) =
    emitBlock 
      [ nLine
      , emit cond
      , nLine
      , goToIf randomLbl
      , nLine
      , emit stmt
      , nLine
      , emitLbl randomLbl
      ]
    where
      randomLbl :: String
      randomLbl = (<> "_if") $ getRandomLbl newStdGen
  
  emit (IfElse (Expr cond) stmt1 stmt2) =
    emitBlock
      [ nLine
      , emit cond
      , goToIfElse randomLbl randomLbl'
      , nLine
      , emitLbl randomLbl
      , emit stmt1
      , nLine
      , emitLbl randomLbl'
      , emit stmt2
      ]
    where
      randomLbl :: String
      randomLbl = (<> "_if") $ getRandomLbl newStdGen

      randomLbl' :: String
      randomLbl' = (<> "_else") $ getRandomLbl newStdGen
  
  emit s = Left $ BadExpression $ "unknown statement: " <> show s

instance Emittable ExprT where
  emit (Var name) = emitNLn $ "mov eax, " <> name
  emit (Const c) = emitNLn "mov eax, " <$*> emit c
  emit (Unary op expr) =
    case op of
      Neg        -> emit expr <$*> emitNLn "neg eax"
      Complement -> emit expr <$*> emitNLn "xor eax, -1"
      Not        -> 
        emitBlock
          [ emit expr
          , emitNLn "cmp eax, 0"
          , emitNLn "sete al"
          ]
  emit (Binary op expr1 expr2) =
    case op of
      Subtract -> emitBinary subOp
      Add      -> emitBinary addOp
      Mod      -> emitBinary modOp
      Multiply -> emitBinary mulOp
      Divide   -> emitBinary divOp
      And      -> emitBinary andOp
      Or       -> emitBinary orOp
      Greater  -> emitBinary greaterOp
      Less     -> emitBinary lessOp
      Equal    -> emitBinary eqOp
    where
      emitBinary :: Either Err String -> Either Err String
      emitBinary f = emitBlock 
        [ emit expr2
        , pushEax
        , emit expr1
        , popEbx
        , f
        ]

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

nLine :: Either Err String
nLine = emitNLn ""

emitLn :: String -> Either Err String
emitLn = Right . ("\t" <>)

emitLbl :: String -> Either Err String
emitLbl = Right . ("\n" <>) . (<> ":")

popEbx :: Either Err String
popEbx = emitNLn "pop ebx"

pushEax :: Either Err String
pushEax = emitNLn "push eax"

-- Basic math functions
subOp :: Either Err String
subOp = emitNLn "sub eax, ebx"

addOp :: Either Err String
addOp = emitNLn "add eax, ebx"

modOp :: Either Err String
modOp =
  emitBlock
    [ emitNLn "xor edx, edx"
    , emitNLn "div ebx"
    , emitNLn "mov eax, edx"
    ]

mulOp :: Either Err String
mulOp = emitNLn "imul ebx"

divOp :: Either Err String
divOp =
  emitNLn "cdq" <$*>
  emitLn "idiv ebx"

andOp :: Either Err String
andOp = emitNLn "and eax, ebx"

orOp :: Either Err String
orOp = emitNLn "or eax, ebx"

eqOp :: Either Err String
eqOp = 
  emitNLn "cmp eax, ebx" <$*>
  emitNLn "sete al"

greaterOp :: Either Err String
greaterOp = 
  emitNLn "cmp eax, ebx" <$*>
  emitNLn "setg al"

lessOp :: Either Err String
lessOp = 
  emitNLn "cmp eax, ebx" <$*>
  emitNLn "setl al"

-- Work with control flow
goToIfElse :: String -> String -> Either Err String
goToIfElse lbl lbl' = 
  emitBlock
    [ emitNLn "cmp eax, 0"
    , emitNLn ("jne " <> lbl)
    , emitNLn ("je " <> lbl')
    ]

goToIf :: String -> Either Err String
goToIf lbl = 
  emitNLn "cmp eax, 0" <$*> 
  emitNLn ("je " <> lbl)

goToReturn :: Either Err String
goToReturn = emitNLn "jmp __ret"

getRandomLbl :: IO StdGen -> String
getRandomLbl gen = 
  "__" <> take 6 (randomRs ('a','z') $ unsafePerformIO gen)
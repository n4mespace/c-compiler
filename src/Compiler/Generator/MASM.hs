{-# LANGUAGE FlexibleInstances #-}

module Compiler.Generator.MASM
  ( generateFile
  , generateString
  ) where

import           Compiler.Generator.Emiters

import           Compiler.Syntax.Control
import           Compiler.Syntax.Error      (Err (..))
import           Compiler.Syntax.Expression
import           Compiler.Types

import           System.Random              (newStdGen)

generateFile :: FilePath -> StmtT -> IO ()
generateFile destination program =
  case addMainFuncCall (emit program) of
    Right asm -> do
      putStrLn "\n{-# GENERATED .ASM #-}"
      putStrLn asm
      destination `writeFile` asm
    Left e -> print e >> fail "asm gen error"

generateString :: StmtT -> IO String
generateString program =
  case addMainFuncCall (emit program) of
    Right asm -> do
      putStrLn "\n{-# GENERATED .ASM #-}"
      putStrLn asm
      return asm
    Left e -> print e >> fail "asm gen error"

-- | Make program capable to generate asm code
class Emittable p where
  emit :: p -> Either ErrT String
  {-# MINIMAL emit #-}

instance Emittable StmtT where
  emit (Block stmts) = emit stmts
  emit (Func func) = emit func
  emit (Assignment assign) = emit assign
  emit (Return Null) = Right ""
  emit (Return (Expr expr)) = emit expr
  emit (If (Expr cond) stmt) =
    emitBlock
      [ nLine
      , emit cond
      , nLine
      , goToIf endLbl
      , nLine
      , emit stmt
      , nLine
      , emitLbl endLbl
      ]
    where
      endLbl :: String
      endLbl = (<> "_endif") $ getRandomLbl newStdGen
  emit (IfElse (Expr cond) stmt1 stmt2) =
    emitBlock
      [ nLine
      , emit cond
      , goToIfElse ifLbl elseLbl
      , nLine
      , emitLbl ifLbl
      , emit stmt1
      , goTo endLbl
      , nLine
      , emitLbl elseLbl
      , emit stmt2
      , nLine
      , emitLbl endLbl
      ]
    where
      ifLbl :: String
      ifLbl = (<> "_if") $ getRandomLbl newStdGen

      elseLbl :: String
      elseLbl = (<> "_else") $ getRandomLbl newStdGen

      endLbl :: String
      endLbl = (<> "_endif") $ getRandomLbl newStdGen
  emit (Expr expr) = emit expr
  emit unknown = Left $ BadExpression $ "unknown statement: " <> show unknown

instance Emittable FuncT where
  emit (DefineFunc _ fName _ fBody@(Block stmts)) =
    emitBlock
      [ nLine
      , emitLbl $ "__func_" <> fName
      , emitNLn $ "enter " <> show (length stmts * 4) <> ", 0"
      , nLine
      , emit fBody
      , nLine
      , emitNLn "leave"
      , emitNLn "ret"
      ]
  emit _ = Right ""


instance Emittable AssignmentT where
  emit (Assign _ name (Expr expr)) = emit expr <$*> movTo name
  emit (ValueAssign name (Expr expr)) = emit expr <$*> movTo name
  emit (EmptyAssign _ _) = Right ""
  emit (OpAssign op name (Expr expr)) =
    emit $ ValueAssign name
         $ Expr $ Binary op (Var name) expr
  emit unknown = Left $ BadExpression $ "Cannot assign: " <> show unknown


instance Emittable ExprT where
  emit (Var name) = emitNLn $ "mov eax, " <> name
  emit (Const c) = emitNLn "mov eax, " <$*> emit c
  emit (CallFunc fName fArgs) =
    if null fArgs
      then emit (reverse fArgs) <$*> emitNLn ("call " <> fName)
      else emitBlock
        [ emit (reverse fArgs)
        , emitNLn ("call " <> fName)
        , emitNLn $ "add esp, " <> show (length fArgs * 4)
        ]
  emit (Unary op expr) =
    case op of
      Neg        -> emit expr <$*> negOp
      Complement -> emit expr <$*> complementOp
      Not        -> emit expr <$*> notOp
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
      emitBinary :: Either ErrT String -> Either ErrT String
      emitBinary f = emitBlock
        [ emit expr2
        , pushEax
        , emit expr1
        , popEbx
        , f
        ]

instance Emittable a => Emittable [a] where
  emit = emitBlock . (emit <$>)

instance Emittable FArgsT where
  emit (Arg expr) = emit expr <$*> pushEax

instance Emittable C where
  emit (INT i) = Right $ show i
  emit (CHAR c) = Right $ show c
  emit (BOOL b) =
    if b
      then Right "1"
      else Right "0"

{-# LANGUAGE FlexibleInstances #-}

module Compiler.Generator.MASM32
  ( generateFile
  , generateString
  ) where

import           Compiler.Generator.Emiters
import           Compiler.Syntax.Expression
import           Compiler.Syntax.Statement
import           Compiler.Types

import           System.Random              (newStdGen)

generateFile :: FilePath -> StmtT -> IO ()
generateFile destination program =
  case runEmitter program of
    Left e -> print e >> fail "asm gen error"
    Right asm -> do
      putStrLn "\n{-# GENERATED .ASM #-}"
      putStrLn asm
      destination `writeFile` asm
      putStrLn "Press ENTER to exit..." <* getLine

generateString :: Either ErrT StmtT -> Either ErrT String
generateString = (>>= runEmitter)

runEmitter :: StmtT -> Either ErrT String
runEmitter = addMainFuncCall . checkBreakAndContinue . emit

-- | Make program capable to generate asm code
class Emittable p where
  emit :: p -> Either ErrT String
  {-# MINIMAL emit #-}


instance Emittable StmtT where
  emit (Block stmts) = emit stmts
  emit (Func func) = emit func
  emit (Assignment assign) = emit assign
  emit (Loop loop) = emit loop
  emit (Return exprOrNull) =
    emitBlock
      [ emit exprOrNull
      , nLine, ret
      ]
  emit (If cond stmt) =
    emitBlock
      [ nLine, emit cond
      , nLine, goToIf endLbl
      , nLine, emit stmt
      , nLine, emitLbl endLbl
      ]
    where
      endLbl :: String
      endLbl = (<> "_endif") $ getRandomLbl newStdGen

  emit (IfElse cond stmt1 stmt2) =
    emitBlock
      [ nLine, emit cond
      , goToIfElse ifLbl elseLbl
      , nLine, emitLbl ifLbl
      , emit stmt1
      , goTo endLbl
      , nLine, emitLbl elseLbl
      , emit stmt2
      , nLine, emitLbl endLbl
      ]
    where
      ifLbl :: String
      ifLbl = (<> "_if") $ getRandomLbl newStdGen

      elseLbl :: String
      elseLbl = (<> "_else") $ getRandomLbl newStdGen

      endLbl :: String
      endLbl = (<> "_endif") $ getRandomLbl newStdGen

  emit (Expr expr) = emit expr
  emit Continue = emitNLn "__continue"
  emit Break = emitNLn "__break"
  emit Null = return ""


instance Emittable FuncT where
  emit (DefineFunc _ fName _ fBody@(Block stmts)) =
    emitBlock
      [ nLine, emitLbl $ "__func_" <> fName
      , emitNLn $ "enter " <> show (length stmts * 4) <> ", 0"
      , nLine, emit fBody
      , case last stmts of
          Return _ -> emitLn ""
          _        -> ret
      ]
  emit _ = return ""


instance Emittable AssignmentT where
  emit (Assign _ name expr) = emit expr <$*> movTo name
  emit (ValueAssign name expr) = emit expr <$*> movTo name
  emit (EmptyAssign _ _) = Right ""
  emit (OpAssign op name expr) =
    emit $ ValueAssign name
         $ Binary op (Var name) expr


instance Emittable ExprT where
  emit (Var name) = emitNLn $ "mov eax, " <> name
  emit (Const c) = emitNLn "mov eax, " <$*> emit c
  emit (CallFunc fName fArgs) =
    if null fArgs
      then emitBlock
        [ nLine, emit fArgs
        , emitNLn $ "call __func_" <> fName
        ]
      else emitBlock
        [ nLine, emit fArgs
        , emitNLn $ "call __func_" <> fName
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


instance Emittable LoopT where
  emit (While cond body) =
    emitLoopBlock loopCond loopEnd
      [ nLine, goTo loopCond
      , nLine, emitLbl loopStart
      , emit body
      , nLine, emitLbl loopCond
      , emit cond
      , goToIfNot loopStart
      , nLine, emitLbl loopEnd
      ]
    where
      loopCond :: String
      loopCond = (<> "_while_cond") $ getRandomLbl newStdGen

      loopStart :: String
      loopStart = (<> "_while_start") $ getRandomLbl newStdGen

      loopEnd :: String
      loopEnd = (<> "_while_end") $ getRandomLbl newStdGen

  emit (For (ForHeader init cond post) body) =
    emitLoopBlock loopCond loopEnd
      [ nLine, emit init
      , nLine, goTo loopCond
      , nLine, emitLbl loopStart
      , emit body
      , nLine, emit post
      , nLine, emitLbl loopCond
      , if cond /= Null
          then emit cond <$*> goToIfNot loopStart
          else goTo loopStart
      , nLine, emitLbl loopEnd
      ]
    where
      loopCond :: String
      loopCond = (<> "_for_cond") $ getRandomLbl newStdGen

      loopStart :: String
      loopStart = (<> "_for_start") $ getRandomLbl newStdGen

      loopEnd :: String
      loopEnd = (<> "_for_end") $ getRandomLbl newStdGen


instance Emittable a => Emittable [a] where
  emit = emitBlock . (emit <$>)


instance Emittable FArgT where
  emit (FArg expr) = emit expr <$*> pushEax


instance Emittable C where
  emit (INT i)  = Right $ show i
  emit (CHAR c) = Right $ show c
  emit (BOOL b) = if b then Right "1" else Right "0"

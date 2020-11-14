{-# LANGUAGE FlexibleInstances #-}

module Compiler.Parser.Grammar where

import           Compiler.Parser.Checkers

import           Compiler.Syntax.Control
import           Compiler.Syntax.Error
import           Compiler.Syntax.Expression

import           Compiler.Types

import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State.Lazy

import qualified Data.Map.Strict                as M
import           Text.Pretty.Simple             (CheckColorTty (CheckColorTty),
                                                 OutputOptions (..),
                                                 defaultOutputOptionsDarkBg,
                                                 pPrintOpt)

checkGrammar :: StmtT -> IO StmtT
checkGrammar ast =
  case runChecker ast initialEnv of
    Left e -> print e >> fail "parser error"
    Right checkedAst -> do
      putStrLn "\n{-# GENERATED AST-TOKENS #-}"
      pretty ast
      return checkedAst
  where
    initialEnv :: GlobalEnv
    initialEnv = (-1, M.singleton (-1, "") (0, False, []))

    opts :: OutputOptions
    opts = defaultOutputOptionsDarkBg
      { outputOptionsIndentAmount = 4
      , outputOptionsPageWidth = 65
      , outputOptionsCompact = True
      }

    pretty :: StmtT -> IO ()
    pretty = pPrintOpt CheckColorTty opts

runChecker :: StmtT -> GlobalEnv -> Either ErrT StmtT
runChecker ast initialEnv = do
  (checkedAst, (_, envMap)) <- runStateT (check ast) initialEnv
  let
    undefinedFunc :: EnvMap
    undefinedFunc = M.filter (\(_, defined, _) -> not defined) envMap

  case envMap M.!? (0, "main") of
    Nothing -> Left $ BadExpression "Cannot find main function"
    Just _ ->
      if M.size undefinedFunc == 1
        then return checkedAst
        else Left $ BadExpression
                  $ "Found undefined objects:" <>
                    concatMap (\(_, name) -> name <> " ")
                              (M.keys undefinedFunc)


-- | Make program capable to be checked
class Checkable p where
  check :: p -> GlobalState p
  {-# MINIMAL check #-}


instance Checkable StmtT where
  check Null = return Null
  check (Func func) = Func <$> check func
  check (Loop loop) = Loop <$> check loop
  check (Assignment assign) = Assignment <$> check assign
  check (Expr expr) = Expr <$> check expr
  check (Return stmt) = Return <$> check stmt
  check (If expr stmt) = 
    If <$> check expr <*> 
           check stmt
  check (IfElse expr stmt1 stmt2) =
    IfElse <$> check expr <*>
               check stmt1 <*>
               check stmt2
  check (Block block) =
    withScope $
      case block of
        []  -> lift $ Left EmptyBlock
        [s] -> Block . (: []) <$> check s
        ss  -> Block <$> traverse check ss


instance Checkable ExprT where
  check (Var vName) = do
    envIdLookup vName funcNothing funcJust
    where
      funcNothing :: GlobalState ExprT
      funcNothing = lift $
        Left $ BadExpression $ "Unknown var: " <> vName

      funcJust :: Env -> GlobalState ExprT
      funcJust (offset, defined, _) =
        if defined
          then return $ Var $ constructAddress offset
          else lift $
            Left $ BadExpression $ "Uninitialized var: " <> vName

  check (CallFunc fName fArgs) = do
    envIdLookup fName funcNothing funcJust
    where
      funcNothing :: GlobalState ExprT
      funcNothing = lift $
        Left $ BadExpression $ "Unknown function: " <> fName

      funcJust :: Env -> GlobalState ExprT
      funcJust (_, _, fParams) =
        if length fParams == length fArgs
          then CallFunc fName <$> check fArgs
          else lift $
            Left $ BadExpression
                 $ "Wrong number of arguments in function: " <> fName <>
                   ". Must be: " <> show (length fParams) <> " args"

  check (Binary op expr1 expr2) =
    Binary op <$> check expr1 <*> check expr2

  check (Unary op expr1) = Unary op <$> check expr1

  check c@(Const _) = return c


instance Checkable AssignmentT where
  check (Assign aType aName expr) = do
    (currScope, envMap) <- get
    let
      funcNothing :: GlobalState AssignmentT
      funcNothing = do
        checkedExpr <- check expr
        modify $ addIdToEnv (currScope, aName) (ebpOffset, True, [])
        return $ Assign aType (constructAddress ebpOffset) checkedExpr

      funcJust :: Env -> GlobalState AssignmentT
      funcJust _ = lift $
        Left $ BadExpression
             $ "Already declared var: " <> aName

      ebpOffset :: EbpOffset
      ebpOffset = getMaxFromMap envMap + 4

    envIdLookup aName funcNothing funcJust

  check (EmptyAssign aType aName) = do
    (currScope, envMap) <- get
    let
      funcNothing :: EbpOffset -> GlobalState AssignmentT
      funcNothing offset = do
        modify $ addIdToEnv (currScope, aName) (offset, False, [])
        return $ EmptyAssign aType (constructAddress offset)

      funcJust :: Env -> GlobalState AssignmentT
      funcJust _ = lift $
        Left $ BadExpression
             $ "Already declared var: " <> aName

      ebpOffset :: EbpOffset
      ebpOffset = getMaxFromMap envMap + 4

    envIdLookup aName (funcNothing 0) funcJust

  check (ValueAssign aName expr) = do
    (currScope, envMap) <- get
    let
      funcNothing :: GlobalState AssignmentT
      funcNothing = lift $
        Left $ BadExpression $ "Unknown var: " <> aName

      funcJust :: Env -> GlobalState AssignmentT
      funcJust (_, False, _) = do
        checkedExpr <- check expr
        modify $ addIdToEnv (currScope, aName) (ebpOffset, True, [])
        return $ ValueAssign (constructAddress ebpOffset) checkedExpr
      funcJust (offset, _, _) =
        ValueAssign (constructAddress offset) <$> check expr

      ebpOffset :: EbpOffset
      ebpOffset = getMaxFromMap envMap + 4

    envIdLookup aName funcNothing funcJust

  check (OpAssign op aName expr) = do
    (currScope, envMap) <- get
    let
      funcNothing :: GlobalState AssignmentT
      funcNothing = lift $
        Left $ BadExpression $ "Unknown var: " <> aName

      funcJust :: Env -> GlobalState AssignmentT
      funcJust (_, False, _) = do
        checkedExpr <- check expr
        modify $ addIdToEnv (currScope, aName) (ebpOffset, True, [])
        return $ OpAssign op (constructAddress ebpOffset) checkedExpr
      funcJust (offset, _, _) =
        OpAssign op (constructAddress offset) <$> check expr

      ebpOffset :: EbpOffset
      ebpOffset = getMaxFromMap envMap + 4

    envIdLookup aName funcNothing funcJust


instance Checkable FuncT where
  check (DefineFunc fType fName fParams fBody) = do
    (currScope, envMap) <- get
    let
      funcNothing :: GlobalState FuncT
      funcNothing = do
        modify $ addIdToEnv (currScope, fName) (0, True, fParams)
        DefineFunc fType fName <$>
                   check fParams <*>
                   check fBody

      funcJust :: Env -> GlobalState FuncT
      funcJust (_, False, fParams') =
        if fParams == fParams'
          then funcNothing
          else lift $
            Left $ BadExpression
                 $ "Different parameters in declaration " <>
                   "and definition " <>
                   "in function: " <> fName
      funcJust _ = lift $
        Left $ BadExpression
             $ "Multiple function definition: " <> fName

    envIdLookup fName funcNothing funcJust

  check (DeclareFunc fType fName fParams) = do
    (currScope, envMap) <- get
    let
      funcNothing :: GlobalState FuncT
      funcNothing = do
        modify $ addIdToEnv (currScope, fName) (0, False, fParams)
        return $ DeclareFunc fType fName fParams

      funcJust :: Env -> GlobalState FuncT
      funcJust _ = lift $
        Left $ BadExpression
             $ "Already declared function: " <> fName

    envIdLookup fName funcNothing funcJust


instance Checkable LoopT where
  check (While cond body) = 
    While <$> check cond <*> withoutScope (check body)
  check (For forHeader body) = undefined

instance Checkable ForHeaderT where
  check (ForHeader init cond post) = undefined

instance Checkable [FArgT] where
  check [] = return []
  check ((FArg arg):args) =
    (:) . FArg <$> check arg <*> check args


instance Checkable [FParamT] where
  check [] = return []
  check ((FParam t pName):params) = do
    (currScope, _) <- get
    let ebpOffset = (length params + 2) * (-4)
    modify $ addIdToEnv (currScope, pName) (ebpOffset, True, [])
    (FParam t (constructAddress ebpOffset) :) <$> check params

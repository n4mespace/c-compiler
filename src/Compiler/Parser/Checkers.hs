{-# LANGUAGE FlexibleInstances #-}

module Compiler.Parser.Checkers where

import           Compiler.Errors
import           Compiler.Parser.Common
import           Compiler.Syntax.Expression
import           Compiler.Syntax.Statement
import           Compiler.Types

import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State.Lazy

import qualified Data.Map.Strict                as M

runChecker :: StmtT -> GlobalEnv -> Either ErrT StmtT
runChecker ast env = do
  (checkedAst, (_, _, envMap)) <- runStateT (check ast) env
  let
    undefObjects :: EnvMap
    undefObjects =
      M.filter (\(_, defined, _, _) -> not defined) $
               M.delete (-1, "") envMap

  case envMap M.!? (0, "main") of
    Nothing -> mainFuncNotFoundErr
    Just _ ->
      if M.size undefObjects == 0
        then return checkedAst
        else undefinedObjectsErr undefObjects


-- | Make program capable to be checked
class Checkable p where
  check :: p -> GlobalState p
  {-# MINIMAL check #-}


instance Checkable StmtT where
  check Null = return Null
  check Break = return Break
  check Continue = return Continue
  check (Func func) = Func <$> check func
  check (Loop loop) = Loop <$> check loop
  check (Assignment assign) = Assignment <$> check assign
  check (Expr expr) = Expr <$> check expr
  check (Return stmt) = Return <$> check stmt
  check (Block []) = lift emptyBlockErr
  check (Block block) = withScope $ Block <$> mapM check block
  check (If expr stmt) = If <$> check expr <*> check stmt
  check (IfElse expr stmt1 stmt2) =
    IfElse <$> check expr <*> check stmt1 <*>
                              check stmt2


instance Checkable ExprT where
  check (Var vName) = do
    let
      funcNothing :: GlobalState ExprT
      funcNothing = lift $ unknownVarErr vName

      funcJust :: Env -> CurrScope -> GlobalState ExprT
      funcJust (offset, defined, _, _) _ =
        if defined
          then return $ Var $ constructAddress offset
          else lift $ uninitVarErr vName

    envIdLookup vName funcNothing funcJust

  check (CallFunc fName fArgs) = do
    let
      funcNothing :: GlobalState ExprT
      funcNothing = lift $ unknownFuncErr fName

      funcJust :: Env -> CurrScope -> GlobalState ExprT
      funcJust (_, _, _, fParams) _ =
        if length fParams == length fArgs
          then CallFunc fName <$> check fArgs
          else lift $ wrongNumArgsErr fName fParams

    envIdLookup fName funcNothing funcJust

  check (Binary op expr1 expr2) =
    Binary op <$> check expr1 <*> check expr2

  check (Unary op expr1) = Unary op <$> check expr1

  check c@(Const _) = return c


instance Checkable AssignmentT where
  check (Assign aType aName expr) = do
    (currScope, currFunc, envMap) <- get
    let
      funcNothing :: GlobalState AssignmentT
      funcNothing = do
        expr' <- check expr
        modify $ addIdToEnv (currScope, aName)
                            (ebpOffset, True, currFunc, [])
        return $ Assign aType (constructAddress ebpOffset) expr'

      funcJust :: Env -> CurrScope -> GlobalState AssignmentT
      funcJust _ updatedScope =
        if updatedScope < currScope
          then funcNothing
          else lift $ alreadyDeclaredVarErr aName

      ebpOffset :: EbpOffset
      ebpOffset = getNextMin envMap currFunc

    envIdLookup aName funcNothing funcJust

  check (EmptyAssign aType aName) = do
    (currScope, currFunc, envMap) <- get
    let
      funcNothing :: EbpOffset -> GlobalState AssignmentT
      funcNothing offset = do
        modify $ addIdToEnv (currScope, aName)
                            (offset, False, currFunc, [])
        return $ EmptyAssign aType (constructAddress offset)

      funcJust :: Env -> CurrScope -> GlobalState AssignmentT
      funcJust _ updatedScope =
        if updatedScope < currScope
          then funcNothing ebpOffset
          else lift $ alreadyDeclaredVarErr aName

      ebpOffset :: EbpOffset
      ebpOffset = getNextMin envMap currFunc

    envIdLookup aName (funcNothing 0) funcJust

  check (ValueAssign aName expr) = do
    (currScope, currFunc, envMap) <- get
    let
      funcNothing :: GlobalState AssignmentT
      funcNothing = lift $ unknownVarErr aName

      funcJust :: Env -> CurrScope -> GlobalState AssignmentT
      funcJust (_, False, _, _) _ = do
        expr' <- check expr
        modify $ addIdToEnv (currScope, aName)
                            (ebpOffset, True, currFunc, [])
        return $ ValueAssign (constructAddress ebpOffset) expr'
      funcJust (offset, _, _, _) _ =
        ValueAssign (constructAddress offset) <$> check expr

      ebpOffset :: EbpOffset
      ebpOffset = getNextMin envMap currFunc

    envIdLookup aName funcNothing funcJust

  check (OpAssign op aName expr) = do
    (currScope, currFunc, envMap) <- get
    let
      funcNothing :: GlobalState AssignmentT
      funcNothing = lift $ unknownVarErr aName

      funcJust :: Env -> CurrScope -> GlobalState AssignmentT
      funcJust (_, False, _, _) _ = do
        checkedExpr <- check expr
        modify $ addIdToEnv (currScope, aName)
                            (ebpOffset, True, currFunc, [])
        return $ OpAssign op (constructAddress ebpOffset) checkedExpr
      funcJust (offset, _, _, _) _ =
        OpAssign op (constructAddress offset) <$> check expr

      ebpOffset :: EbpOffset
      ebpOffset = getNextMin envMap currFunc

    envIdLookup aName funcNothing funcJust


instance Checkable FuncT where
  check (DefineFunc fType fName fParams fBody) = do
    (currScope, _, _) <- get
    let
      funcNothing :: GlobalState FuncT
      funcNothing = do
        modify $ addIdToEnv (currScope, fName)
                            (0, True, "", fParams)
        DefineFunc fType fName <$>
                   check fParams <*>
                   check fBody

      funcJust :: Env -> CurrScope -> GlobalState FuncT
      funcJust (_, False, _, fParams') _ =
        if fParams == fParams'
          then funcNothing
          else lift $ differentParamsErr fName
      funcJust _ _ = lift $ alreadyDefinedFuncErr fName

    modify $ setFuncScope fName
    envIdLookup fName funcNothing funcJust

  check (DeclareFunc fType fName fParams) = do
    (currScope, _, _) <- get
    let
      funcNothing :: GlobalState FuncT
      funcNothing = do
        modify $ addIdToEnv (currScope, fName)
                            (0, False, "", fParams)
        return $ DeclareFunc fType fName fParams

      funcJust :: Env -> CurrScope -> GlobalState FuncT
      funcJust _ _ = lift $ alreadyDeclaredFuncErr fName

    envIdLookup fName funcNothing funcJust


instance Checkable LoopT where
  check (While cond body) =
    While <$> check cond <*> check body

  check (For forHeader body) =
    For <$> check forHeader <*> check body


instance Checkable ForHeaderT where
  check (ForHeader init cond post) = do
    let
      init' :: GlobalState StmtT
      init' = case init of
        Null                  -> check Null
        assign@(Assignment _) -> check assign
        _                     -> lift initClauseErr

      cond' :: GlobalState StmtT
      cond' = case cond of
        Null          -> check Null
        expr@(Expr _) -> check expr
        _             -> lift condClauseErr

      post' :: GlobalState StmtT
      post' = case post of
        Null                              -> check Null
        assign@(Assignment ValueAssign{}) -> check assign
        assign@(Assignment OpAssign{})    -> check assign
        _                                 -> lift postClauseErr

    withScope $
      ForHeader <$> init' <*> cond' <*> post'


instance Checkable FArgT where
  check (FArg expr) = FArg <$> check expr


instance Checkable [FArgT] where
  check [] = return []
  check (arg:args) =
    (:) <$> check arg <*> check args


instance Checkable [FParamT] where
  check [] = return []
  check ((FParam t pName):params) = do
    (currScope, currFunc, _) <- get
    let
      ebpOffset :: EbpOffset
      ebpOffset = (length params + 2) * 4

    modify $ addIdToEnv (currScope, pName)
                        (ebpOffset, True, currFunc, [])

    (FParam t (constructAddress ebpOffset) :) <$> check params

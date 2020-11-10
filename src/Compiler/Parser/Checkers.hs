module Compiler.Parser.Checkers where

import           Compiler.Syntax.Control
import           Compiler.Syntax.Error
import           Compiler.Syntax.Expression
import           Compiler.Types

import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict                as M

checker :: StmtT -> GlobalState
checker code = do
  g <- get
  let (scope, env) = g

  case code of
    Func func -> checkerFunc g func

    Block block ->
      withScope $
        case block of
          []  -> lift $ Left BadReturn
          [s] -> checker s
          ss  -> Block <$> traverse checker ss

    Assignment assignment ->
      case assignment of
        Assign aType aName expr -> do
          envIdLookup g aName (funcNothing ebpOffset) funcJust
          where
            funcNothing :: EbpOffset -> GlobalState
            funcNothing offset = do
              checkedExpr <- checker expr
              modify $ addIdToEnv (scope, aName) offset
              return $ Assignment $ Assign aType (constructAddress offset) checkedExpr

            funcJust :: EbpOffset -> GlobalState
            funcJust _ = do
              (updatedScope, _) <- get
              if updatedScope < scope
                then funcNothing ebpOffset
                else lift $ Left $
                    BadExpression $ "Already declared var: " <> aName

            ebpOffset :: EbpOffset
            ebpOffset = getMaxFromMap env + 4

        EmptyAssign aType aName -> do
          envIdLookup g aName (funcNothing 0) funcJust
          where
            funcNothing :: EbpOffset -> GlobalState
            funcNothing offset = do
              modify $ addIdToEnv (scope, aName) offset
              return $ Assignment $ EmptyAssign aType (constructAddress offset)

            funcJust :: EbpOffset -> GlobalState
            funcJust _ = do
              (updatedScope, _) <- get
              if updatedScope <= scope
                then
                  funcNothing ebpOffset
                else
                  lift $ Left $ BadExpression $ "Already declared var: " <> aName

            ebpOffset :: EbpOffset
            ebpOffset = getMaxFromMap env + 4

        ValueAssign aName expr -> do
          envIdLookup g aName funcNothing funcJust
          where
            funcNothing :: GlobalState
            funcNothing =
              lift $ Left $ BadExpression $ "Unknown var: " <> aName

            funcJust :: EbpOffset -> GlobalState
            funcJust 0 = do
                checkedExpr <- checker expr
                modify $ addIdToEnv (scope, aName) ebpOffset
                return $ Assignment $ ValueAssign (constructAddress ebpOffset) checkedExpr
            funcJust n =
               Assignment . ValueAssign (constructAddress n) <$> checker expr

            ebpOffset :: EbpOffset
            ebpOffset = getMaxFromMap env + 4

        OpAssign op aName expr -> do
          envIdLookup g aName funcNothing funcJust
          where
            funcNothing :: GlobalState
            funcNothing =
              lift $ Left $ BadExpression $ "Unknown var: " <> aName

            funcJust :: EbpOffset -> GlobalState
            funcJust 0 = do
                checkedExpr <- checker expr
                modify $ addIdToEnv (scope, aName) ebpOffset
                return $ Assignment $ OpAssign op (constructAddress ebpOffset) checkedExpr
            funcJust n =
               Assignment . OpAssign op (constructAddress n) <$> checker expr

            ebpOffset :: EbpOffset
            ebpOffset = getMaxFromMap env + 4

    Expr v@(Var _) ->
      case checkerExpr g v of
        Left e    -> lift $ Left e
        Right var -> return $ Expr var

    Expr (Binary op expr1 expr2) ->
      case Binary op <$> checkerExpr g expr1
                     <*> checkerExpr g expr2 of
        Left e    -> lift $ Left e
        Right bin -> return $ Expr bin

    Expr (Unary op expr) ->
      case Unary op <$> checkerExpr g expr of
        Left e  -> lift $ Left e
        Right v -> return $ Expr v

    Expr f@(CallFunc _ _) ->
      case checkerExpr g f of
        Left e     -> lift $ Left e
        Right func -> return $ Expr func

    If expr stmt -> If <$> checker expr <*> checker stmt

    IfElse expr stmt1 stmt2 ->
      IfElse <$> checker expr <*> checker stmt1 <*> checker stmt2

    Return stmt -> Return <$> checker stmt

    stmt -> return stmt

checkerFunc :: GlobalEnv -> FuncT -> GlobalState
checkerFunc g@(scope, _) func =
  case func of
    DefineFunc fType fName fParams fBody ->
      envIdLookup g fName funcNothing funcJust
      where
        funcNothing :: GlobalState
        funcNothing = do
          modify $ addIdToEnv (scope, fName) 0
          Func <$> (DefineFunc fType fName
                <$> checkerParams g fParams
                <*> checker fBody)

        funcJust :: EbpOffset -> GlobalState
        funcJust (-1) = funcNothing
        funcJust _ =
          lift $ Left $ BadExpression $ "Mulpiple function definition: " <> fName

    DeclareFunc fType fName fParams ->
      envIdLookup g fName funcNothing funcJust
      where
        funcNothing :: GlobalState
        funcNothing = do
          modify $ addIdToEnv (scope, fName) (-1)
          return $ Func $ DeclareFunc fType fName fParams

        funcJust :: EbpOffset -> GlobalState
        funcJust _ =
          lift $ Left $ BadExpression $ "Already declared function: " <> fName

checkerArgs :: GlobalEnv -> [FArgsT] -> Either ErrT [FArgsT]
checkerArgs _ [] = Right []
checkerArgs g ((Arg arg):args) =
  (:) . Arg <$> checkerExpr g arg <*> checkerArgs g args

checkerParams :: GlobalEnv
              -> [FParamsT]
              -> StateT GlobalEnv (Either ErrT) [FParamsT]
checkerParams _ [] = return []
checkerParams g@(scope, env) (p@(Param _ pName):params) = do
  modify $ addIdToEnv (scope, pName) $ getMaxFromMap env + 4
  (p :) <$> checkerParams g params

envIdLookup :: GlobalEnv
            -> Name
            -> GlobalState
            -> (EbpOffset -> GlobalState)
            -> GlobalState
envIdLookup (scope, env) aName funcNothing funcJust =
  case M.lookup (scope, aName) env of
    Nothing -> do
      if scope /= 0
        then envIdLookup (scope - 1, env) aName funcNothing funcJust
        else funcNothing
    Just offset -> funcJust offset

checkerExpr :: GlobalEnv -> ExprT -> Either ErrT ExprT
checkerExpr (scope, env) v@(Var vName) =
  case M.lookup (scope, vName) env of
    Nothing ->
      if scope /= 0
        then checkerExpr (scope - 1, env) v
        else Left $ BadExpression $ "Unknown var: " <> vName
    Just 0  -> Left $ BadExpression $ "Uninitialized var: " <> vName
    Just n  -> Right $ Var $ constructAddress n
checkerExpr g@(scope, _) f@(CallFunc fName fArgs) = go g f
  where
    go :: GlobalEnv -> ExprT -> Either ErrT ExprT
    go (scope', env') f' = 
      case M.lookup (scope', fName) env' of
        Nothing ->
          if scope /= 0
            then go (scope' - 1, env') f'
            else Left $ BadExpression $ "Unknown function: " <> fName
        Just _ -> CallFunc fName <$> checkerArgs g fArgs
checkerExpr g (Binary op expr1 expr2) =
  Binary op <$> checkerExpr g expr1 <*> checkerExpr g expr2
checkerExpr g (Unary op expr) = Unary op <$> checkerExpr g expr
checkerExpr _ expr = Right expr

-- Helpers
constructAddress :: EbpOffset -> String
constructAddress = ("dword ptr [ebp + " <>) . (<> "]") . show

getMaxFromMap :: Ord v => M.Map k v -> v
getMaxFromMap m = maximum (snd <$> M.toList m)

addIdToEnv :: (Scope, Name) -> EbpOffset -> GlobalEnv -> GlobalEnv
addIdToEnv scopedName offset (scope, s) = (scope, M.insert scopedName offset s)

nextScope :: GlobalEnv -> GlobalEnv
nextScope (scope, e) = (scope + 1, e)

prevScope :: GlobalEnv -> GlobalEnv
prevScope (scope, e) = (scope - 1, e)

withScope :: GlobalState -> GlobalState
withScope g = modify nextScope >> g <* modify prevScope

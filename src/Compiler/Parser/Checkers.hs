module Compiler.Parser.Checkers where

import           Compiler.Syntax.Control        (Assignment (..), Func (..),
                                                 Name, Stmt (..))
import           Compiler.Syntax.Error
import           Compiler.Syntax.Expression
import           Compiler.Types

import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict                as M

checker :: StmtT -> GlobalState
checker code = do
  (currScope, env) <- get

  case code of
    Func function ->
      case function of
        DefineFunc fType fName fParams fBody ->
          envIdLookup currScope env fName funcNothing funcJust
          where
            funcNothing :: GlobalState
            funcNothing = do
              modify $ addIdToEnv (currScope, fName) 0
              Func . DefineFunc fType fName fParams <$> checker fBody

            funcJust :: EbpOffset -> GlobalState
            funcJust (-1) = funcNothing
            funcJust _ =
              lift $ Left $ BadExpression $ "Mulpiple function definition: " <> fName

        DeclareFunc fType fName fParams ->
          envIdLookup currScope env fName funcNothing funcJust
          where
            funcNothing :: GlobalState
            funcNothing = do
              modify $ addIdToEnv (currScope, fName) (-1)
              return $ Func $ DeclareFunc fType fName fParams

            funcJust :: EbpOffset -> GlobalState
            funcJust _ =
              lift $ Left $ BadExpression $ "Already declared function: " <> fName

        CallFunc fName fParams ->
          envIdLookup currScope env fName funcNothing funcJust
          where
            funcNothing :: GlobalState
            funcNothing =
              lift $ Left $ BadExpression $ "Unknown func: " <> fName

            funcJust :: EbpOffset -> GlobalState
            funcJust _ =
              return $ Func $ CallFunc fName fParams

    Block block ->
      withScope $
        case block of
          []  -> lift $ Left BadReturn
          [s] -> checker s
          ss  -> Block <$> traverse checker ss

    Assignment assignment ->
      case assignment of
        Assign aType aName expr -> do
          envIdLookup currScope env aName (funcNothing ebpOffset) funcJust
          where
            funcNothing :: EbpOffset -> GlobalState
            funcNothing offset = do
              checkedExpr <- checker expr
              modify $ addIdToEnv (currScope, aName) offset
              return $ Assignment $ Assign aType (constructAddress offset) checkedExpr

            funcJust :: EbpOffset -> GlobalState
            funcJust _ = do
              (updatedScope, _) <- get
              if updatedScope < currScope
                then funcNothing ebpOffset
                else lift $ Left $
                    BadExpression $ "Already declared var: " <> aName

            ebpOffset :: EbpOffset
            ebpOffset = getMaxFromMap env + 4

        EmptyAssign aType aName -> do
          envIdLookup currScope env aName (funcNothing 0) funcJust
          where
            funcNothing :: EbpOffset -> GlobalState
            funcNothing offset = do
              modify $ addIdToEnv (currScope, aName) offset
              return $ Assignment $ EmptyAssign aType (constructAddress offset)

            funcJust :: EbpOffset -> GlobalState
            funcJust _ = do
              (updatedScope, _) <- get
              if updatedScope <= currScope
                then
                  funcNothing ebpOffset
                else
                  lift $ Left $ BadExpression $ "Already declared var: " <> aName

            ebpOffset :: EbpOffset
            ebpOffset = getMaxFromMap env + 4

        ValueAssign aName expr -> do
          envIdLookup currScope env aName funcNothing funcJust
          where
            funcNothing :: GlobalState
            funcNothing =
              lift $ Left $ BadExpression $ "Unknown var: " <> aName

            funcJust :: EbpOffset -> GlobalState
            funcJust 0 = do
                checkedExpr <- checker expr
                modify $ addIdToEnv (currScope, aName) ebpOffset
                return $ Assignment $ ValueAssign (constructAddress ebpOffset) checkedExpr
            funcJust n =
               Assignment . ValueAssign (constructAddress n) <$> checker expr

            ebpOffset :: EbpOffset
            ebpOffset = getMaxFromMap env + 4

        OpAssign op aName expr -> do
          envIdLookup currScope env aName funcNothing funcJust
          where
            funcNothing :: GlobalState
            funcNothing =
              lift $ Left $ BadExpression $ "Unknown var: " <> aName

            funcJust :: EbpOffset -> GlobalState
            funcJust 0 = do
                checkedExpr <- checker expr
                modify $ addIdToEnv (currScope, aName) ebpOffset
                return $ Assignment $ OpAssign op (constructAddress ebpOffset) checkedExpr
            funcJust n =
               Assignment . OpAssign op (constructAddress n) <$> checker expr

            ebpOffset :: EbpOffset
            ebpOffset = getMaxFromMap env + 4

    Expr (Var vName) -> do
      envIdLookup currScope env vName funcNothing funcJust
      where
        funcNothing :: GlobalState
        funcNothing =
          lift $ Left $ BadExpression $ "Unknown var: " <> vName

        funcJust :: EbpOffset -> GlobalState
        funcJust 0 =
          lift $ Left $ BadExpression $ "Uninitialized var: " <> vName
        funcJust n =
          return $ Expr . Var $ constructAddress n

    Expr (Binary op expr1 expr2) ->
      case Binary op <$> exprLookup (currScope, env) expr1
                     <*> exprLookup (currScope, env) expr2 of
        Left e    -> lift $ Left e
        Right bin -> return $ Expr bin

    Expr (Unary op expr) ->
      case Unary op <$> exprLookup (currScope, env) expr of
        Left e  -> lift $ Left e
        Right v -> return $ Expr v

    If expr stmt -> If <$> checker expr <*> checker stmt

    IfElse expr stmt1 stmt2 ->
      IfElse <$> checker expr <*> checker stmt1 <*> checker stmt2

    Return stmt -> Return <$> checker stmt

    stmt -> return stmt

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

exprLookup :: GlobalEnv -> ExprT -> Either ErrT ExprT
exprLookup (scope, env) (Var vName) =
  case M.lookup (scope, vName) env of
    Nothing -> do
      if scope /= 0
        then exprLookup (scope - 1, env) (Var vName)
        else Left $ BadExpression $ "Unknown var: " <> vName
    Just 0  -> Left $ BadExpression $ "Uninitialized var: " <> vName
    Just n  -> Right $ Var $ constructAddress n
exprLookup st (Binary op' expr1' expr2') =
  Binary op' <$> exprLookup st expr1' <*> exprLookup st expr2'
exprLookup st (Unary op' expr') = Unary op' <$> exprLookup st expr'
exprLookup _ rest = Right rest

envIdLookup :: CurrScope
             -> EnvMap
             -> Name
             -> GlobalState
             -> (EbpOffset -> GlobalState)
             -> GlobalState
envIdLookup scope env aName funcNothing funcJust =
  case M.lookup (scope, aName) env of
    Nothing -> do
      if scope /= 0
        then envIdLookup (scope - 1) env aName funcNothing funcJust
        else funcNothing
    Just offset -> funcJust offset

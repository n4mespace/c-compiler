module Compiler.Parser.Checkers where

import           Compiler.Syntax.Control
import           Compiler.Syntax.Error
import           Compiler.Syntax.Expression
import           Compiler.Types

import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict                as M

checkerProgram :: StmtT -> GlobalEnv -> Either ErrT StmtT
checkerProgram ast initialEnv = do
  (checkedAst, (_, envMap)) <- runStateT (checker ast) initialEnv
  let undefinedFunc :: EnvMap
      undefinedFunc = M.filter (\(_, defined, _, _) -> not defined) envMap

      countUndefinedFunc :: Int
      countUndefinedFunc = M.size undefinedFunc

  if countUndefinedFunc == 1
    then return checkedAst
    else Left $ BadExpression
              $ "Found undefined functions:" <>
                 concatMap (\(_, name) -> name <> " ")
                           (M.keys undefinedFunc)

checker :: StmtT -> GlobalState
checker code = do
  g <- get
  let (currScope, envMap) = g

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
          envIdLookup g aName funcNothing funcJust
          where
            funcNothing :: GlobalState
            funcNothing = do
              checkedExpr <- checker expr
              modify $ addIdToEnv
                       (currScope, aName)
                       (ebpOffset, True, [], [])
              return $ Assignment $ Assign aType (constructAddress ebpOffset) checkedExpr

            funcJust :: Env -> GlobalState
            funcJust _ = do
              (updatedScope, _) <- get
              if updatedScope < currScope
                then funcNothing
                else lift $ Left $ BadExpression
                          $ "Already declared var: " <> aName

            ebpOffset :: EbpOffset
            ebpOffset = getMaxFromMap envMap + 4

        EmptyAssign aType aName -> do
          envIdLookup g aName (funcNothing 0) funcJust
          where
            funcNothing :: EbpOffset -> GlobalState
            funcNothing offset = do
              modify $ addIdToEnv
                       (currScope, aName)
                       (offset, False, [], [])
              return $ Assignment
                     $ EmptyAssign aType (constructAddress offset)

            funcJust :: Env -> GlobalState
            funcJust _ = do
              (updatedScope, _) <- get
              if updatedScope <= currScope
                then funcNothing ebpOffset
                else lift $ Left $ BadExpression
                          $ "Already declared var: " <> aName

            ebpOffset :: EbpOffset
            ebpOffset = getMaxFromMap envMap + 4

        ValueAssign aName expr -> do
          envIdLookup g aName funcNothing funcJust
          where
            funcNothing :: GlobalState
            funcNothing =
              lift $ Left $ BadExpression $ "Unknown var: " <> aName

            funcJust :: Env -> GlobalState
            funcJust (_, False, _, _) = do
                checkedExpr <- checker expr
                modify $ addIdToEnv
                         (currScope, aName)
                         (ebpOffset, True, [], [])
                return $ Assignment $ ValueAssign (constructAddress ebpOffset) checkedExpr
            funcJust (offset, _, _, _) =
               Assignment . ValueAssign (constructAddress offset)
                                     <$> checker expr

            ebpOffset :: EbpOffset
            ebpOffset = getMaxFromMap envMap + 4

        OpAssign op aName expr -> do
          envIdLookup g aName funcNothing funcJust
          where
            funcNothing :: GlobalState
            funcNothing =
              lift $ Left $ BadExpression
                          $ "Unknown var: " <> aName

            funcJust :: Env -> GlobalState
            funcJust (_, False, _, _) = do
                checkedExpr <- checker expr
                modify $ addIdToEnv
                         (currScope, aName)
                         (ebpOffset, True, [], [])
                return $ Assignment $ OpAssign op (constructAddress ebpOffset) checkedExpr
            funcJust (offset, _, _, _) =
              Assignment . OpAssign
                op (constructAddress offset) <$> checker expr

            ebpOffset :: EbpOffset
            ebpOffset = getMaxFromMap envMap + 4

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
checkerFunc g@(currScope, _) func =
  case func of
    DefineFunc fType fName fParams fBody ->
      envIdLookup g fName funcNothing funcJust
      where
        funcNothing :: GlobalState
        funcNothing = do
          modify $ addIdToEnv
                   (currScope, fName)
                   (0, True, fParams, [])
          Func <$> (DefineFunc fType fName
                <$> checkerParams g fParams
                <*> checker fBody)

        funcJust :: Env -> GlobalState
        funcJust (_, False, _, _) = funcNothing
        funcJust _ =
          lift $ Left $ BadExpression
                      $ "Multiple function definition: " <> fName

    DeclareFunc fType fName fParams ->
      envIdLookup g fName funcNothing funcJust
      where
        funcNothing :: GlobalState
        funcNothing = do
          modify $ addIdToEnv
                   (currScope, fName)
                   (0, False, fParams, [])
          Func <$> (DeclareFunc fType fName
                <$> checkerParams g fParams)

        funcJust :: Env -> GlobalState
        funcJust _ =
          lift $ Left $ BadExpression
                      $ "Already declared function: " <> fName

checkerArgs :: GlobalEnv -> [FArgsT] -> Either ErrT [FArgsT]
checkerArgs _ [] = Right []
checkerArgs g ((Arg arg):args) =
  (:) . Arg <$> checkerExpr g arg <*> checkerArgs g args

checkerParams :: GlobalEnv
              -> [FParamsT]
              -> StateT GlobalEnv (Either ErrT) [FParamsT]
checkerParams _ [] = return []
checkerParams g@(currScope, env) (p@(Param _ pName):params) = do
  modify $ addIdToEnv
           (currScope, pName)
           (ebpOffset, True, [], [])
  (p :) <$> checkerParams g params
  where
    ebpOffset :: EbpOffset
    ebpOffset = getMaxFromMap env + 4

envIdLookup :: GlobalEnv
            -> Name
            -> GlobalState
            -> (Env -> GlobalState)
            -> GlobalState
envIdLookup (currScope, envMap) aName funcNothing funcJust =
  case M.lookup (currScope, aName) envMap of
    Nothing -> do
      if currScope /= 0
        then envIdLookup (currScope - 1, envMap) aName funcNothing funcJust
        else funcNothing
    Just env -> funcJust env

checkerExpr :: GlobalEnv -> ExprT -> Either ErrT ExprT
checkerExpr (currScope, envMap) v@(Var vName) =
  case M.lookup (currScope, vName) envMap of
    Nothing ->
      if currScope /= 0
        then checkerExpr (currScope - 1, envMap) v
        else Left $ BadExpression $ "Unknown var: " <> vName
    Just (offset, defined, _, _) ->
      if defined
        then Right $ Var $ constructAddress offset
        else Left $ BadExpression $ "Uninitialized var: " <> vName

checkerExpr g@(scope, _) f@(CallFunc fName fArgs) = go g f
  where
    go :: GlobalEnv -> ExprT -> Either ErrT ExprT
    go (currScope', envMap') f' =
      case M.lookup (currScope', fName) envMap' of
        Nothing ->
          if scope /= 0
            then go (currScope' - 1, envMap') f'
            else Left $ BadExpression $ "Unknown function: " <> fName
        Just _ ->
          CallFunc fName <$> checkerArgs g fArgs

checkerExpr g (Binary op expr1 expr2) =
  Binary op <$> checkerExpr g expr1 <*> checkerExpr g expr2

checkerExpr g (Unary op expr) = Unary op <$> checkerExpr g expr
checkerExpr _ expr = Right expr

-- Helpers
constructAddress :: EbpOffset -> String
constructAddress = ("dword ptr [ebp + " <>) . (<> "]") . show

getMaxFromMap :: EnvMap -> EbpOffset
getMaxFromMap envMap = maximum $
  (\(offset, _, _, _) -> offset) <$> M.elems envMap

addIdToEnv :: ScopedName
           -> Env
           -> GlobalEnv
           -> GlobalEnv
addIdToEnv scopedName (offset, defined, params, args) (currScope, env) =
  ( currScope
  , M.insert
      scopedName
      (offset, defined, params, args)
      env
  )

nextScope :: GlobalEnv -> GlobalEnv
nextScope (currScope, envMap) = (currScope + 1, envMap)

prevScope :: GlobalEnv -> GlobalEnv
prevScope (currScope, envMap) = (currScope - 1, envMap)

withScope :: GlobalState -> GlobalState
withScope st = modify nextScope >> st <* modify prevScope

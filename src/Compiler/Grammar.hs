module Compiler.Grammar
  ( checkGrammar
  , Err(..)
  ) where

import           Compiler.Syntax.Control        (ExprT, Name, Stmt (..), StmtT,
                                                 Type (..))
import           Compiler.Syntax.Expression
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict                as M
import           Text.Pretty.Simple             (pPrint)

-- | Counter for variable address generation
type EbpOffset = Int

-- | Every block of code adds 1 to scope counter
type Scope = Int

-- | Counter for scope depth
type CurrScope = Int

-- | Map for identifiers
type EnvMap = M.Map (Scope, Name) EbpOffset

-- | Global state map
type GlobalEnv = (CurrScope, EnvMap)

type GlobalState = StateT GlobalEnv (Either Err) StmtT

data Err
  = UnExpectedType Type
  | BadReturn
  | CannotAssignTo String Type
  | TypesMissmatch Type Type
  | BadExpression String
  | ReservedName String
  deriving (Show)

checkGrammar :: StmtT -> IO StmtT
checkGrammar parsedProgram = do
  case checkerProgram of
    Left e -> print e >> fail "parser error"
    Right checkedProgram -> do
      putStrLn "\n{-# GENERATED AST-TOKENS #-}"
      pPrint parsedProgram
      return checkedProgram
  where
    checkerProgram :: Either Err StmtT
    checkerProgram = evalStateT (checker parsedProgram) initialState

    initialState :: GlobalEnv
    initialState = (-1, M.singleton (-1, "") 0)

checker :: StmtT -> GlobalState
checker code = do
  (currScope, env) <- get

  case code of
    Func fType fName fParams fBody -> 
      Func fType fName fParams <$> checker fBody

    Block block ->
      withScope $
        case block of
          [] -> lift $ Left BadReturn
          [s] -> checker s
          ss -> Block <$> traverse checker ss

    Assign aType aName expr -> do
      envVarLookup currScope env aName (funcNothing ebpOffset) funcJust
      where
        funcNothing :: EbpOffset -> GlobalState
        funcNothing offset = do
          checkedExpr <- checker expr
          modify $ addVarToEnv (currScope, aName) offset
          return $ Assign aType (constructAddress offset) checkedExpr
  
        funcJust :: EbpOffset -> GlobalState
        funcJust _ = do
          (updatedScope, _) <- get
          if updatedScope <= currScope
            then
              funcNothing ebpOffset
            else
              lift $ Left $ BadExpression $ "already declared var: " <> aName

        ebpOffset :: EbpOffset
        ebpOffset = getMaxFromMap env + 4

    EmptyAssign aType aName -> do
      envVarLookup currScope env aName (funcNothing 0) funcJust
      where
        funcNothing :: EbpOffset -> GlobalState
        funcNothing offset = do
          modify $ addVarToEnv (currScope, aName) offset
          return $ EmptyAssign aType (constructAddress offset)

        funcJust :: EbpOffset -> GlobalState
        funcJust _ = do
          (updatedScope, _) <- get
          if updatedScope <= currScope
            then
              funcNothing ebpOffset
            else
              lift $ Left $ BadExpression $ "already declared var: " <> aName

        ebpOffset :: EbpOffset
        ebpOffset = getMaxFromMap env + 4
    
    ValueAssign aName expr -> do
      envVarLookup currScope env aName funcNothing funcJust
      where
        funcNothing :: GlobalState
        funcNothing = 
          lift $ Left $ BadExpression $ "unknown var: " <> aName

        funcJust :: EbpOffset -> GlobalState
        funcJust 0 = do
            checkedExpr <- checker expr
            modify $ addVarToEnv (currScope, aName) ebpOffset
            return $ ValueAssign (constructAddress ebpOffset) checkedExpr
        funcJust n =
          ValueAssign (constructAddress n) <$> checker expr

        ebpOffset :: EbpOffset
        ebpOffset = getMaxFromMap env + 4
    
    Expr (Var vName) -> do
      envVarLookup currScope env vName funcNothing funcJust
      where
        funcNothing :: GlobalState
        funcNothing = 
          lift $ Left $ BadExpression $ "unknown var: " <> vName
        
        funcJust :: EbpOffset -> GlobalState
        funcJust 0 =
          lift $ Left $ BadExpression $ "uninitialized var: " <> vName
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

addVarToEnv :: (Scope, Name) -> EbpOffset -> GlobalEnv -> GlobalEnv
addVarToEnv scopedName offset (scope, s) = (scope, M.insert scopedName offset s)

nextScope :: GlobalEnv -> GlobalEnv
nextScope (scope, e) = (scope + 1, e)

prevScope :: GlobalEnv -> GlobalEnv
prevScope (scope, e) = (scope - 1, e)

withScope :: GlobalState -> GlobalState
withScope g = modify nextScope >> g <* modify prevScope

exprLookup :: GlobalEnv -> ExprT -> Either Err ExprT
exprLookup (scope, env) (Var vName) =
  case M.lookup (scope, vName) env of
    Nothing -> do
      if scope /= 0
        then exprLookup (scope - 1, env) (Var vName)
        else Left $ BadExpression $ "unknown var: " <> vName
    Just 0  -> Left $ BadExpression $ "uninitialized var: " <> vName
    Just n  -> Right $ Var $ constructAddress n
exprLookup st (Binary op' expr1' expr2') =
  Binary op' <$> exprLookup st expr1' <*> exprLookup st expr2'
exprLookup st (Unary op' expr') = Unary op' <$> exprLookup st expr'
exprLookup _ rest = Right rest

envVarLookup :: CurrScope 
             -> EnvMap 
             -> Name 
             -> GlobalState 
             -> (EbpOffset -> GlobalState)
             -> GlobalState
envVarLookup scope env aName funcNothing funcJust = 
  case M.lookup (scope, aName) env of
    Nothing -> do
      if scope /= 0
        then envVarLookup (scope - 1) env aName funcNothing funcJust 
        else funcNothing
    Just offset -> funcJust offset

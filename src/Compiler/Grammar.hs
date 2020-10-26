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

type EbpOffset = Int

type GlobalEnv = M.Map Name EbpOffset

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
    Right program -> do
      putStrLn "\n{-# GENERATED AST-TOKENS #-}"
      pPrint parsedProgram
      return program
  where
    checkerProgram :: Either Err StmtT
    checkerProgram = evalStateT (checker parsedProgram) (M.singleton "" 0)

checker :: StmtT -> GlobalState
checker code = do
  env <- get
  
  case code of
    Func fType fName fParams fBody -> do
      case rType of
        Right t ->
          if fType == t
            then Func fType fName fParams <$> checker fBody
            else lift $ Left $ TypesMissmatch t INT_T
        Left e ->
          lift $ Left e
      where 
        findReturn :: StmtT -> [StmtT]
        findReturn (Block [s]) = findReturn s
        findReturn (Block (s:ss)) = merge (findReturn s) (findReturn $ Block ss)
        findReturn (Return stmt) = [stmt]
        findReturn Null = [Null]
        findReturn _ = []

        returnExprs :: [StmtT]
        returnExprs = findReturn fBody

        parseReturn :: StmtT -> Either Err Type
        parseReturn Null = Left BadReturn
        parseReturn _    = Right INT_T

        rType :: Either Err Type
        rType =
          case length returnExprs of
            1 -> parseReturn (head returnExprs)
            _ -> Left BadReturn

    Block [] -> lift $ Left BadReturn
    Block [s] -> checker s
    Block ss -> Block <$> traverse checker ss
    
    Assign aType aName expr -> do
      case M.lookup aName env of
        Nothing -> do
          checkedExpr <- checker expr
          modify $ M.insert aName ebpOffset
          return $ Assign aType (constructAddress ebpOffset) checkedExpr
        Just _ ->
          lift $ Left $ BadExpression $ "already declared var: " <> aName
      where 
        ebpOffset :: EbpOffset
        ebpOffset = getMaxFromMap env + 4
    
    EmptyAssign aType aName -> do
      case M.lookup aName env of
        Nothing -> do
          modify $ M.insert aName 0
          return $ EmptyAssign aType (constructAddress 0)
        Just _ ->
          lift $ Left $ BadExpression $ "already declared var: " <> aName
    
    ValueAssign aName expr -> do
      case M.lookup aName env of
        Nothing -> lift $ Left $ BadExpression $ "unknown var: " <> aName
        Just 0 -> do
          checkedExpr <- checker expr
          modify $ M.insert aName ebpOffset
          return $ ValueAssign (constructAddress ebpOffset) checkedExpr
        Just n -> ValueAssign (constructAddress n) <$> checker expr
      where 
        ebpOffset :: EbpOffset
        ebpOffset = getMaxFromMap env + 4
    
    Expr (Binary op expr1 expr2) ->
      case Binary op <$> 
           lookupCheck env expr1 <*> 
           lookupCheck env expr2 of
        Left e -> lift $ Left e
        Right bin -> return $ Expr bin
    
    Expr (Var varName) -> do
      case M.lookup varName env of
        Nothing -> lift $ Left $ BadExpression $ "unknown var: " <> varName
        Just 0 -> lift $ Left $ BadExpression $ "uninitialized var: " <> varName
        Just n -> return $ Expr . Var $ constructAddress n
    
    Expr (Unary Neg expr) ->
      case Unary Neg <$> lookupCheck env expr of
        Left e -> lift $ Left e
        Right v -> return $ Expr v

    Expr (Unary Complement expr) ->
      case Unary Complement <$> lookupCheck env expr of
        Left e -> lift $ Left e
        Right v -> return $ Expr v

    Return stmt -> Return <$> checker stmt
    stmt -> return stmt

-- Helpers
merge :: [a] -> [a] -> [a]
merge [] ys     = ys
merge (x:xs) ys = x : merge ys xs

constructAddress :: EbpOffset -> String
constructAddress = ("dword ptr [ebp + " <>) . (<> "]") . show

getMaxFromMap :: Ord v => M.Map k v -> v
getMaxFromMap m = maximum (snd <$> M.toList m)

lookupCheck :: GlobalEnv -> ExprT -> Either Err ExprT
lookupCheck env (Var varName) =
  case M.lookup varName env of
    Nothing -> Left $ BadExpression $ "unknown var: " <> varName
    Just 0  -> Left $ BadExpression $ "uninitialized var: " <> varName
    Just n  -> Right $ Var $ constructAddress n
lookupCheck env (Binary op' expr1' expr2') = Binary op' 
                                         <$> lookupCheck env expr1' 
                                         <*> lookupCheck env expr2'
lookupCheck _ rest = Right rest

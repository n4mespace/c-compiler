module Compiler.Grammar
  ( checkGrammar
  , Err(..)
  )

where

import           Compiler.Syntax.Arithmetic     (AExpr (..))
import           Compiler.Syntax.Control        (Expr (..), Name, Stmt (..),
                                                 Type (..))
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict                  as M
import           Text.Pretty.Simple             (pPrint)

type EbpOffset = Int
type GlobalEnv = M.Map Name EbpOffset
type GlobalState = StateT GlobalEnv (Either Err) Stmt

data Err
  = UnExpectedType Type
  | BadReturn
  | CannotAssignTo String Type
  | TypesMissmatch Type Type
  | BadExpression String
  | ReservedName String
  deriving (Show)

checkGrammar :: Stmt -> IO Stmt
checkGrammar parsedProgram = do
  case checkerProgram of
    Left e -> print e >> fail "parser error"
    Right program -> do
      putStrLn "\n{-# GENERATED AST-TOKENS #-}"
      pPrint parsedProgram
      return program
  where
    checkerProgram :: Either Err Stmt
    checkerProgram = evalStateT (checker parsedProgram) (M.singleton "" 0)

checker :: Stmt -> GlobalState
checker code = do
  env <- get

  case code of
    Func fType fName fParams body -> do
      case fType of
        INT -> case returnType of
          Right INT -> returnFunc
          Left e    -> lift $ Left e
          _         -> lift $ Left $ UnExpectedType INT
        CHAR -> case returnType of
          Right INT -> returnFunc
          Left e    -> lift $ Left e
          _         -> lift $ Left $ UnExpectedType CHAR
        VOID -> case returnType of
          Right VOID -> returnFunc
          Left e     -> lift $ Left e
          _          -> lift $ Left $ UnExpectedType VOID
      where
        findReturn :: Stmt -> [Stmt]
        findReturn (Block [s]) = findReturn s
        findReturn (Block (s:ss)) = merge (findReturn s)  (findReturn $ Block ss)
        findReturn (Return stmt) = [stmt]
        findReturn Null = [Null]
        findReturn _ = []

        returnExprs :: [Stmt]
        returnExprs = findReturn body

        parseReturn :: Stmt -> Either Err Type
        parseReturn Null = Right VOID
        parseReturn _    = Right INT

        returnType :: Either Err Type
        returnType = case length returnExprs of
          0 -> Right VOID
          1 -> parseReturn (head returnExprs)
          _ -> Left BadReturn

        returnFunc :: GlobalState 
        returnFunc = Func fType fName fParams <$> checker body

    Block [] -> lift $ Left BadReturn
    Block [s] -> checker s
    Block ss -> Block <$> traverse checker ss

    Assign aType aName expr -> do
      case M.lookup aName env of
        Nothing -> do
          checkedExpr <- checker expr
          modify $ M.insert aName ebpOffset
          return $ Assign aType (constructAddress ebpOffset) checkedExpr   
        Just _  -> lift $ Left $ BadExpression $ "already declared var: " <> aName
      where
        ebpOffset :: EbpOffset
        ebpOffset = getMaxFromMap env + 4

    EmptyAssign aType aName -> do
      case M.lookup aName env of
        Nothing -> do
          modify $ M.insert aName 0
          return $ EmptyAssign aType (constructAddress 0)
        Just _  -> lift $ Left $ BadExpression $ "already declared var: " <> aName

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

    Expr (ArExpr (ABinary op expr1 expr2)) ->
      case ABinary op <$> lookupCheck env expr1 <*> lookupCheck env expr2 of
        Left e -> lift $ Left e
        Right v -> return $ Expr . ArExpr $ v
        
    Expr (ArExpr (Var varName)) -> do
      case M.lookup varName env of
        Nothing -> lift $ Left $ BadExpression $ "unknown var: " <> varName
        Just 0 -> lift $ Left $ BadExpression $ "uninitialized var: " <> varName
        Just n -> return $ Expr . ArExpr . Var $ constructAddress n

    Expr (ArExpr (Neg expr)) -> 
      case Neg <$> lookupCheck env expr of
        Left e -> lift $ Left e
        Right v -> return $ Expr . ArExpr $ v
    
    Expr (ArExpr (Complement expr)) -> 
      case Complement <$> lookupCheck env expr of
        Left e -> lift $ Left e
        Right v -> return $ Expr . ArExpr $ v

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

lookupCheck :: GlobalEnv -> AExpr -> Either Err AExpr
lookupCheck env (Var varName) =
  case M.lookup varName env of
    Nothing -> Left $ BadExpression $ "unknown var: " <> varName
    Just 0 -> Left $ BadExpression $ "uninitialized var: " <> varName
    Just n -> Right $ Var $ constructAddress n
lookupCheck env (ABinary op' expr1' expr2') = ABinary op' 
                                          <$> lookupCheck env expr1' 
                                          <*> lookupCheck env expr2'
lookupCheck env (Neg expr1') = Neg <$> lookupCheck env expr1'
lookupCheck env (Complement expr1') = Complement <$> lookupCheck env expr1' 
lookupCheck _ rest = Right rest
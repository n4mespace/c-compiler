{-# LANGUAGE LambdaCase #-}

module Compiler.Grammar
  ( checkGrammar
  , Err(..)
  ) where


import           Compiler.Syntax.Arithmetic     (AExpr (..))
import           Compiler.Syntax.Control        (Expr (..), Name, Stmt (..),
                                                 Type (..))
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State.Lazy
import qualified Data.Map.Lazy as M
import           Text.Pretty.Simple             (pPrint)


type GlobalEnv = M.Map Name Stmt
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
  case checkerResult of
    Left e -> print e >> fail "parser error"
    Right _ -> do
      putStrLn "{-# GENERATED AST-TOKENS #-}"
      pPrint parsedProgram
      return parsedProgram
  where
    checkerResult :: Either Err Stmt
    checkerResult = evalStateT (checker parsedProgram) M.empty

checker :: Stmt -> GlobalState
checker code = do
  env <- get

  case code of
    func@(Func fType fName fParams body) -> do
      case M.lookup fName env of
        Nothing -> modify $ M.insert fName func
        Just _ -> lift $ Left $ BadExpression $ "cannot reassign a function: " <> fName

      case fType of
        INT -> case returnType of
          Right INT -> checker body
          Left e    -> lift $ Left e
          _         -> lift $ Left $ UnExpectedType INT
        CHAR -> case returnType of
          Right INT -> checker body
          Left e    -> lift $ Left e
          _         -> lift $ Left $ UnExpectedType CHAR
        VOID -> case returnType of
          Right VOID -> checker body
          Left e     -> lift $ Left e
          _          -> lift $ Left $ UnExpectedType VOID
      where 
        findReturn :: Stmt -> [Stmt]
        findReturn (Block [s]) = findReturn s
        findReturn (Block (s:ss)) = merge (findReturn s) (findReturn $ Block ss)
        findReturn (Return stmt) = [stmt]
        findReturn ReturnNull = [ReturnNull]
        findReturn _ = []
        
        returnExprs :: [Stmt]
        returnExprs = findReturn body
        
        parseReturn :: Stmt -> Either Err Type
        parseReturn ReturnNull = Right VOID
        parseReturn _          = Right INT
        
        returnType :: Either Err Type
        returnType = case length returnExprs of
          0 -> Right VOID
          1 -> parseReturn (head returnExprs)
          _ -> Left BadReturn

    Block [] -> lift $ Left BadReturn
    Block [s] -> checker s
    Block (s:ss) -> Block 
                <$> sequence [ checker s, 
                               checker $ Block ss ]
      
    assign@(Assign aType aName expr) -> do
      modify $ M.insert aName assign

      case exprType of
        Right t
          | aType == VOID -> lift $ Left $ CannotAssignTo aName VOID
          | t == CHAR || t == INT -> checker expr
          | otherwise -> lift $ Left $ TypesMissmatch aType t
        Left e -> lift $ Left e
      where 
        findType :: Stmt -> Either Err Type
        findType (Expr (ArExpr (IntConst _))) = Right INT
        findType _ = Left $ CannotAssignTo aName aType
        
        exprType :: Either Err Type
        exprType = findType expr

    binary@(Expr (ArExpr (ABinary _ expr1 expr2))) -> 
         lookupCheck expr1 
      >> lookupCheck expr2
      where
        lookupCheck :: AExpr -> GlobalState 
        lookupCheck (Var varname) = 
          case M.lookup varname env of
            Nothing -> lift $ Left $ BadExpression $ "unknown var: " <> varname
            Just _ -> return binary
        lookupCheck _ = return binary

    Return stmt -> checker stmt

    stmt -> return stmt

merge :: [a] -> [a] -> [a]
merge [] ys     = ys
merge (x:xs) ys = x : merge ys xs

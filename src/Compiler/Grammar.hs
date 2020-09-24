{-# LANGUAGE LambdaCase #-}

module Compiler.Grammar
  ( checkGrammar
  , Err (..)
  )
where

import           Compiler.Parser
import           Compiler.Syntax.Control  (Name, Stmt (..), Type (..), Expr (..))
import           Compiler.Syntax.Arithmetic (AExpr (..))
import           Control.Monad.State.Lazy
import           Data.Either
import           Data.Map.Lazy            (Map, empty)
import           Data.Monoid


-- data Id = Id {
--     getType :: Type
--   , getName :: Name
-- }

-- type GlobalEnv = Map Id Stmt
-- type GlobalState = StateT GlobalEnv (Either Err) Stmt

data Err 
  = UnExpectedType Type
  | BadReturn
  deriving Show

checkGrammar :: Stmt -> IO (Either Err Stmt)
checkGrammar = return . checker

checker :: Stmt -> Either Err Stmt
checker = \case
  func@(Func fType fName fParams body) ->
    case fType of
      INT -> case l of
        1 -> case returnType of
          Right INT -> Right func
          _ -> Left $ UnExpectedType INT
        _ -> Left BadReturn
      CHAR -> case l of
        1 -> case returnType of
          Right CHAR -> Right func
          _ -> Left $ UnExpectedType CHAR
        _ -> Left BadReturn
      VOID -> case l of
        0 -> Right func
        1 -> case returnType of
          Right VOID -> Right func
          _ -> Left $ UnExpectedType VOID
        _ -> Left BadReturn
    where
      findReturn :: Stmt -> [Stmt]
      findReturn (Block (s:[])) = findReturn s
      findReturn (Block (s:ss)) = merge (findReturn s) (findReturn $ Block ss)
      findReturn (Return stmt) = [stmt]
      findReturn (ReturnNull) = [ReturnNull]
      findReturn _ = []

      returnExprs :: [Stmt]
      returnExprs = findReturn body

      parseReturn :: Stmt -> Either Err Type
      parseReturn (Expr (ArExpr (CharConst _))) = Right CHAR
      parseReturn (Expr (ArExpr (IntConst _))) = Right INT
      parseReturn (ReturnNull) = Right VOID
      parseReturn _ = Left BadReturn

      l :: Int
      l = length returnExprs

      returnType :: Either Err Type
      returnType = case l of
        1 -> parseReturn (head returnExprs)
        _ -> return VOID
  
  Block [] -> Left BadReturn
  Block (s:[]) -> checker s
  Block (s:ss) -> checker $ Block ss

  assign@(Assign aType aName expr) ->
    Right assign
  stmt ->
    Right stmt


merge :: [a] -> [a] -> [a]  
merge [] ys = ys
merge (x:xs) ys = x : merge ys xs
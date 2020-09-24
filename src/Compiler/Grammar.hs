{-# LANGUAGE LambdaCase #-}

module Compiler.Grammar
  ( checkGrammar
  )
where

import           Compiler.Parser
import           Compiler.Syntax.Control  (Name, Stmt (..), Type (..))
import           Control.Monad.State.Lazy
import           Data.Either
import           Data.Map.Lazy            (Map, empty)
import           Data.Monoid


data Id = Id {
    getType :: Type
  , getName :: Name
}

-- type GlobalEnv = Map Id Stmt
-- type GlobalState = StateT GlobalEnv (Either Err) Stmt

data Err = TypeMismatch Type Type
  deriving Show

checkGrammar :: Stmt -> IO (Either Err Stmt)
checkGrammar = return . (\case
    func@(Func fType fName fParams body) ->
      Right func
    assign@(Assign aType aName expt) ->
      Right assign
    stmt ->
      Right stmt)
    
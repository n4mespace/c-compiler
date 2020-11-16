module Compiler.Parser.Common where

import           Compiler.Types

import           Control.Monad.Trans.State.Lazy

import qualified Data.Map.Strict                as M


initialEnv :: GlobalEnv
initialEnv = (-1, "", M.singleton (-1, "") (0, False, "", []))

-- Helpers
envIdLookup :: String
            -> GlobalState a
            -> (Env -> CurrScope -> GlobalState a)
            -> GlobalState a
envIdLookup name funcNothing funcJust = do
  (currScope, currFunc, envMap) <- get
  let
    go scope =
      case M.lookup (scope, name) envMap of
        Nothing ->
          if scope /= 0
            then go $ scope - 1
            else funcNothing
        Just env@(_, _, func, _) ->
          if currFunc == func || func == ""
            then funcJust env scope
            else funcNothing
  go currScope

constructAddress :: EbpOffset -> String
constructAddress offset =
  if offset > 0
    then "dword ptr [ebp - " <> show offset <> "]"
    else "dword ptr [ebp + " <> show (offset * (-1)) <> "]"

getNextMax :: EnvMap -> CurrFuncName -> EbpOffset
getNextMax envMap currFunc =
  if M.null currEnvMap
    then 4
    else 4 + maximum (
      (\(offset, _, _, _) -> offset) <$> M.elems currEnvMap)
  where
    currEnvMap :: EnvMap
    currEnvMap = M.filter
      (\(_, _, func, _) -> currFunc == func) envMap

addIdToEnv :: ScopedName
           -> Env
           -> GlobalEnv
           -> GlobalEnv
addIdToEnv scopedName env (currScope, currFunc, envMap) =
  ( currScope
  , currFunc
  , M.insert
      scopedName
      env
      envMap
  )

changeScope :: Int -> GlobalEnv -> GlobalEnv
changeScope n (scope, func, env) = (scope + n, func, env)

nextScope :: GlobalEnv -> GlobalEnv
nextScope = changeScope 1

prevScope :: GlobalEnv -> GlobalEnv
prevScope = changeScope (-1)

withScopeN :: Int -> GlobalState a -> GlobalState a
withScopeN n st =
  modify (changeScope n) >> st <* modify (changeScope $ negate n)

withScope :: GlobalState a -> GlobalState a
withScope = withScopeN 1

setFuncScope :: CurrFuncName -> GlobalEnv -> GlobalEnv
setFuncScope currFunc (scope, _, env) = (scope, currFunc, env)

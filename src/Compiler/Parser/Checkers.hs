module Compiler.Parser.Checkers where

import           Compiler.Types

import           Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict                as M


-- Helpers
envIdLookup :: String
            -> GlobalState a
            -> (Env -> GlobalState a)
            -> GlobalState a
envIdLookup name funcNothing funcJust = do
  (currScope, envMap) <- get
  let
    go scope =
      case M.lookup (scope, name) envMap of
        Nothing ->
          if scope /= 0
            then go $ scope - 1
            else funcNothing
        Just env -> funcJust env
  go currScope

constructAddress :: EbpOffset -> String
constructAddress offset =
  if offset > 0
    then "dword ptr [ebp - " <> show offset <> "]"
    else "dword ptr [ebp + " <> show (offset * (-1)) <> "]"

getMaxFromMap :: EnvMap -> EbpOffset
getMaxFromMap envMap = maximum $
  (\(offset, _, _) -> offset) <$> M.elems envMap

addIdToEnv :: ScopedName
           -> Env
           -> GlobalEnv
           -> GlobalEnv
addIdToEnv scopedName (offset, defined, params) (currScope, envMap) =
  ( currScope
  , M.insert
      scopedName
      (offset, defined, params)
      envMap
  )

nextScope :: GlobalEnv -> GlobalEnv
nextScope (currScope, envMap) = (currScope + 1, envMap)

prevScope :: GlobalEnv -> GlobalEnv
prevScope (currScope, envMap) = (currScope - 1, envMap)

withScope :: GlobalState a -> GlobalState a
withScope st = modify nextScope >> st <* modify prevScope

withoutScope :: GlobalState a -> GlobalState a
withoutScope st = modify prevScope >> st <* modify nextScope

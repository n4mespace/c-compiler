module Compiler.Parser.Checkers where

import           Compiler.Types

import           Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict                as M


-- Helpers
-- envIdLookup :: String
--             -> GlobalState a
--             -> (Env -> GlobalState a)
--             -> GlobalState a
-- envIdLookup name funcNothing funcJust = do
--   (currScope, envMap) <- get
--   case M.lookup (currScope, name) envMap of
--     Nothing -> do
--       if currScope /= 0
--         then withScopeBack $
--           envIdLookup name funcNothing funcJust
--         else funcNothing
--     Just env -> funcJust env
envIdLookup :: GlobalEnv
            -> String
            -> GlobalState a
            -> (Env -> GlobalState a)
            -> GlobalState a
envIdLookup (currScope, envMap) aName funcNothing funcJust =
  case M.lookup (currScope, aName) envMap of
    Nothing -> do
      if currScope /= 0
        then envIdLookup (currScope - 1, envMap) aName funcNothing funcJust
        else funcNothing
    Just env -> funcJust env

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

withScopeBack :: GlobalState a -> GlobalState a
withScopeBack st = modify prevScope >> st <* modify nextScope

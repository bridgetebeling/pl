-- src/Environment.hs
module Environment (
    Env,
    emptyEnv,
    lookup,
    bindName
) where

import qualified Data.List (lookup)
import Prelude hiding (lookup)
import Expr (Value)

type Env = [(String, Value)]

emptyEnv :: Env
emptyEnv = []

lookup :: String -> Env -> Maybe Value
lookup = Data.List.lookup

bindName :: String -> Value -> Env -> Env
bindName name val env = (name, val) : env

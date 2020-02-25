module ModuleGroup where

import           Syn
import qualified Canonical                     as Can
import qualified Syn.Typed                     as T

data ModuleGroup exp ty =
  ModuleGroup (Module_ Can.Name exp ty) -- ^the module
              [Module_ Can.Name exp ty] -- ^its dependencies
    deriving (Show)
type UntypedModuleGroup = ModuleGroup Can.Exp Can.Type
data TypedModuleGroup = TypedModuleGroup T.Module [T.Module] deriving Show

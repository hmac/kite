module ModuleGroup where

import           Syn
import qualified Canonical                     as Can
import qualified Syn.Typed                     as T

--                             the module   its dependencies
data ModuleGroup exp ty = ModuleGroup (Module_ Can.Name exp ty) [Module_ Can.Name exp ty]
  deriving (Show)
type UntypedModuleGroup = ModuleGroup Can.Exp Can.Type
data TypedModuleGroup = TypedModuleGroup T.Module [T.Module] deriving Show

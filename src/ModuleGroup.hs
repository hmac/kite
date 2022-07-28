module ModuleGroup where

import qualified Canonical                     as Can
import           Data.Name
import           Syn
import qualified Syn.Typed                     as T

-- | A module group is a module together with its dependencies (other modules).
data ModuleGroup exp ty = ModuleGroup (Module_ Name exp ty)
                                      [Module_ Name exp ty]
  deriving Show
type UntypedModuleGroup = ModuleGroup Can.Exp Can.Type
data TypedModuleGroup = TypedModuleGroup T.Module [T.Module]
  deriving Show

module ModuleGroup where

import qualified Canonical                     as Can
import           Data.Name
import           Syn
import qualified Syn.Typed                     as T

data ModuleGroup exp ty = ModuleGroup
  { rootModule   :: Module_ Name exp ty
  , dependencies :: [Module_ Name exp ty]
  }
  deriving Show
type UntypedModuleGroup = ModuleGroup Can.Exp Can.Type
data TypedModuleGroup = TypedModuleGroup T.Module [T.Module]
  deriving Show

module ExpandImports
  ( expandImports
  , Error(..)
  ) where

-- Given a module, and its dependencies, converts any ImportAll items into
-- explicit ImportSome items by finding all the matching constructors for each
-- type.

import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import qualified Prim
import           Syn
import           Util

data Error = CannotFindModule PkgModuleName -- ^ importing module
                                            PkgModuleName -- ^ module we can't find
  deriving (Eq, Show)

expandImports
  :: MonadError Error m => Module -> [Module] -> m (Module, [Module])
expandImports m deps = do
  expanded <- go (m : reverse deps)
  let mExpanded    = head expanded
      depsExpanded = tail expanded
  pure (mExpanded, reverse depsExpanded)
 where
  go []       = pure []
  go (n : ns) = do
    n'  <- expandAllImports n ns
    ns' <- go ns
    pure (n' : ns')

expandAllImports :: MonadError Error m => Module -> [Module] -> m Module
expandAllImports modul deps = do
  let imps = moduleImports modul
  imps' <- mapM (expand (moduleName modul) deps) imps
  pure modul { moduleImports = imps' }

-- We don't support expanding imports for Kite.Prim, since it's not a "real"
-- module.
expand :: MonadError Error m => PkgModuleName -> [Module] -> Import -> m Import
expand _ _ imp | importName imp == Prim.name = pure imp
expand modulName deps imp =
  let matchingModule = find ((== importName imp) . moduleName) deps
  in  case matchingModule of
        Just m ->
          pure imp { importItems = map (expandItem m) (importItems imp) }
        Nothing -> throwError $ CannotFindModule modulName (importName imp)

expandItem :: Module -> ImportItem -> ImportItem
expandItem importedModule = \case
  i@ImportSingle{} -> i
  i@ImportSome{}   -> i
  ImportAll { importItemName = n } ->
    ImportSome n (fromMaybe [] (lookup n (moduleExports importedModule)))

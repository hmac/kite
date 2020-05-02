module ExpandImports where

-- Given a module, and its dependencies, converts any ImportAll items into
-- explicit ImportSome items by finding all the matching constructors for each
-- type.

import           Syn
import           Util

data Error = CannotFindModule ModuleName -- ^ importing module
                              ModuleName -- ^ module we can't find
  deriving (Eq, Show)

expandImports
  :: Module Syn -> [Module Syn] -> Either Error (Module Syn, [Module Syn])
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

expandAllImports :: Module Syn -> [Module Syn] -> Either Error (Module Syn)
expandAllImports modul deps = do
  let imps = moduleImports modul
  imps' <- mapM (expand (moduleName modul) deps) imps
  pure modul { moduleImports = imps' }

expand :: ModuleName -> [Module Syn] -> Import -> Either Error Import
expand modulName deps imp =
  let matchingModule = find ((== importName imp) . moduleName) deps
  in  case matchingModule of
        Just m ->
          Right imp { importItems = map (expandItem m) (importItems imp) }
        Nothing -> Left $ CannotFindModule modulName (importName imp)

expandItem :: Module Syn -> ImportItem -> ImportItem
expandItem importedModule = \case
  i@ImportSingle{} -> i
  i@ImportSome{}   -> i
  ImportAll { importItemName = n } ->
    ImportSome n (fromMaybe [] (lookup n (moduleExports importedModule)))

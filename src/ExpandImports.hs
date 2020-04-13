module ExpandImports where

-- Given a module, and its dependencies, converts any ImportAll items into
-- explicit ImportSome items by finding all the matching constructors for each
-- type.

import           Syn
import           Util

expandImports :: Module Syn -> [Module Syn] -> (Module Syn, [Module Syn])
expandImports m deps = let (m' : deps') = go (m : deps) in (m', deps')
 where
  go []       = []
  go (n : ns) = mapImports (expand ns) n : go ns

mapImports :: (Import -> Import) -> Module Syn -> Module Syn
mapImports f modul = modul { moduleImports = map f (moduleImports modul) }

expand :: [Module Syn] -> Import -> Import
expand deps imp =
  let matchingModule = find ((== importName imp) . moduleName) deps
  in  case matchingModule of
        Just m  -> imp { importItems = map (expandItem m) (importItems imp) }
        Nothing -> imp

expandItem :: Module Syn -> ImportItem -> ImportItem
expandItem importedModule = \case
  i@ImportSingle{} -> i
  i@ImportSome{}   -> i
  ImportAll { importItemName = n } ->
    ImportSome n (fromMaybe [] (lookup n (moduleExports importedModule)))
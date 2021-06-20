module Data.Name.Gen where

import           Hedgehog                       ( Gen )
import qualified Hedgehog.Gen                  as G
import qualified Hedgehog.Range                as R

import           Data.Name

genName :: Gen Name
genName = G.choice
  [Local <$> genLowerRawName, TopLevel <$> genPkgModuleName <*> genLowerRawName]

-- Uppercase names are data constructors, so are always qualified
genModuleName :: Gen ModuleName
genModuleName = ModuleName <$> G.list (R.linear 1 3) genUpperString

genPkgModuleName :: Gen PkgModuleName
genPkgModuleName = PkgModuleName <$> genPackageName <*> genModuleName

genPackageName :: Gen PackageName
genPackageName = PackageName <$> G.list (R.linear 1 8) G.lower

genLowerRawName :: Gen RawName
genLowerRawName = Name <$> genLowerString

genLowerString :: Gen String
genLowerString = do
  c  <- G.lower
  cs <- G.list (R.linear 0 5) G.alphaNum
  pure (c : cs)

genUpperString :: Gen String
genUpperString = do
  c  <- G.upper
  cs <- G.list (R.linear 0 10) G.alphaNum
  pure (c : cs)

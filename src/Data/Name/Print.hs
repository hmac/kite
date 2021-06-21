module Data.Name.Print where

import           Data.Name
import           Data.Text.Prettyprint.Doc

printName :: Name -> Doc a
printName (Local (Name n)) = pretty n
printName (TopLevel pkgModName (Name n)) =
  hcat $ punctuate "." [pretty (show pkgModName), pretty n]

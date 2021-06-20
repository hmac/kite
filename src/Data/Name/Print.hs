module Data.Name.Print where

import           Data.Name
import           Data.Text.Prettyprint.Doc

printName :: Name -> Doc a
printName (Local (Name n)) = pretty n
printName (TopLevel (ModuleName parts) (Name n)) =
  hcat $ punctuate "." (map pretty (parts ++ [n]))

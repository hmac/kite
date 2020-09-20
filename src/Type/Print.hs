module Type.Print where

import           Data.Text.Prettyprint.Doc
import           Type
import           Data.Name
import           Data.List                      ( intersperse )

printModuleName :: ModuleName -> Doc a
printModuleName (ModuleName names) = hcat (map pretty (intersperse "." names))

printName :: Name -> Doc a
printName (Local (Name n)) = pretty n
printName (TopLevel moduleName (Name n)) =
  printModuleName moduleName <> "." <> pretty n

printLocatedError :: LocatedError -> Doc a
printLocatedError (LocatedError moduleName err) = vsep
  [ "In the definition"
  <+> maybe "<unknown>" printName moduleName
  <+> "I encountered the following error:"
  , printError err
  ]

printError :: Error -> Doc a
printError = pretty . show

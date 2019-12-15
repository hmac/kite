module LC.Print where

import Prelude hiding (print)
import           Data.Text.Prettyprint.Doc

import           LC
import Data.Name
import Canonical (Name(..))
import ELC (Constant(..), Con(..))


-- A simple function to print LC expressions.
-- In future we probably want to implicitly call show on any result value and
-- just display the resulting string.
print :: LC.Exp -> Doc a
print = \case
  Const c _ -> printConstant c
  Var n -> printName n
  Cons c args -> let n = name c
                  in printName n <+> hsep (map print args)
  Bottom s -> "error:" <+> pretty s
  App _ _ -> "<unevaluated application>"
  Abs _ _ -> "<function>"
  Let {} -> "<let>"
  Fail -> "<pattern match failure>"
  _ -> "<invalid expression>"

printConstant :: Constant -> Doc a
printConstant = \case
  Int i -> pretty i
  String s -> "\"" <> pretty s <> "\""
  Float f -> pretty f
  Prim _ -> "<builtin>"

printName :: Name -> Doc a
printName (Local (Name n)) = pretty n
printName (TopLevel (ModuleName parts) (Name n)) =
  hcat $ punctuate "." (map pretty (parts ++ [n]))

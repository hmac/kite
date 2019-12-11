module LC.Print where

import Prelude hiding (print)
import           Data.Text.Prettyprint.Doc

import           LC
import Syntax (Name(..))
import ELC (Constant(..), Con(..))


-- A simple function to print LC expressions.
-- In future we probably want to implicitly call show on any result value and
-- just display the resulting string.
print :: LC.Exp -> Doc a
print = \case
  Const c -> printConstant c
  Var (Name n) -> pretty n
  Cons c args -> let Name n = name c
                  in pretty n <+> hsep (map print args)
  Bottom s -> "error:" <+> pretty s
  App _ _ -> "<unevaluated application>"
  Abs _ _ -> "<function>"
  Let {} -> "<let>"
  Fail -> "<pattern match failure>"
  _ -> "<invalid expression>"

printConstant :: Constant -> Doc a
printConstant = undefined

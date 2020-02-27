module LC.Print where

import           Prelude                 hiding ( print )
import           Data.Text.Prettyprint.Doc

import           LC
import           Data.Name
import           Canonical                      ( Name(..) )
import           ELC                            ( Constant(..)
                                                , Con(..)
                                                )


-- A simple function to print LC expressions.
-- In future we probably want to implicitly call show on any result value and
-- just display the resulting string.
print :: LC.Exp -> Doc a
print = print' Root

data Context = Root | AppL | AppR

print' :: Context -> LC.Exp -> Doc a
print' ctx expr = case (ctx, expr) of
  (_   , Const c _  ) -> printConstant c
  (_   , Var n      ) -> printName n
  (_   , Cons c args) -> printName (conName c) <+> hsep (map print args)
  (_   , Bottom s   ) -> "error:" <+> pretty s
  (AppL, App a b    ) -> parens (print' Root (App a b))
  (AppR, App a b    ) -> print' Root (App a b)
  (_   , App a b    ) -> print' AppL a <+> print' AppR b
  (_   , Abs x e    ) -> "\\" <> printName x <+> "->" <+> print' Root e
  (_   , Let{}      ) -> "<let>"
  (_   , Fail       ) -> "<pattern match failure>"
  (_   , Fatbar a b ) -> print' Root a <+> "|" <+> print' Root b
  (_   , _          ) -> "<invalid expression>"

printConstant :: Constant -> Doc a
printConstant = \case
  Int    i -> pretty i
  String s -> "\"" <> pretty s <> "\""
  Prim   _ -> "<builtin>"

printName :: Name -> Doc a
printName (Local (Name n)) = pretty n
printName (TopLevel (ModuleName parts) (Name n)) =
  hcat $ punctuate "." (map pretty (parts ++ [n]))

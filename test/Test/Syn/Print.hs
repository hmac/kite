module Test.Syn.Print
  ( test
  )
where

import           Test.Hspec
import Syn.Print
import           Syn
import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc (Doc)

import Prelude hiding (mod)

printMaybe :: Maybe (Doc a) -> Doc a
printMaybe = fromMaybe mempty

test :: Spec
test = parallel $ do
  describe "printing types" $ do
    it "prints simple types" $ do
      show (printType (TyVar "a" `fn` TyVar "b")) `shouldBe` "a -> b"
    it "prints type applications" $ do
      show (printType (TyCon "f" [TyVar "a", TyVar "b"])) `shouldBe` "f a b"
    it "prints nested type applications" $ do
      show (printType (TyCon "t" [TyCon "m" [TyVar "a"]])) `shouldBe` "t (m a)"
      show (printType (TyCon "Reader" [TyVar "m", TyVar "a"])) `shouldBe` "Reader m a"
      show (printType (TyCon "a" [TyCon "b" [TyCon "c" [TyVar "d"]]])) `shouldBe` "a (b (c d))"
    it "prints type applications with arrows (1)" $ do
      show (printType (TyVar "a" `fn` TyCon "f" [TyVar "a"])) `shouldBe` "a -> f a"
    it "prints type applications with arrows (2)" $ do
      show (printType ((TyCon "A" [TyVar "a"]) `fn` ((TyCon "f" [TyVar "a"])))) `shouldBe` "A a -> f a"
    it "prints type applications with arrows (3)" $ do
      show (printType ((TyCon "A" [] `fn` TyCon "A" []) `fn` TyCon "A" [])) `shouldBe` "(A -> A) -> A"
    it "prints tuple types" $ do
      show (printType (TyTuple [TyVar "a", TyVar "b"])) `shouldBe` "(a, b)"
    it "prints large tuple types on multiple lines" $ do
      show (printType (TyTuple [TyVar "AVeryLongType"
                               , TyVar "AVeryLongType"
                               , TyVar "AVeryLongType"
                               , TyVar "AVeryLongType"
                               , TyVar "AVeryLongType"
                               , TyVar "AVeryLongType"
                               ])) `shouldBe` "( AVeryLongType\n, AVeryLongType\n, AVeryLongType\n, AVeryLongType\n, AVeryLongType\n, AVeryLongType )"
  describe "printing module" $ do
    it "prints module metadata correctly" $ do
      let meta = [("hi", "there"), ("how", "are you")]
       in show (printMaybe (printMetadata meta)) `shouldBe` "---\nhi: there\nhow: are you\n---"
    it "prints module name correctly" $ do
      let name = ModuleName ["Data", "List", "NonEmpty"]
      show (printModName name) `shouldBe` "module Data.List.NonEmpty"
    it "prints module exports correctly" $ do
      let exports = [("fun1", []), ("SomeType", []), ("OtherType", ["SomeConstructor"])]
      show (printMaybe (printModExports exports)) `shouldBe` "(fun1, SomeType, OtherType(SomeConstructor))"
    it "prints an empty module correctly" $ do
      let mod = Module { moduleName = "Data.Text"
                       , moduleExports = [("Text", [])]
                       , moduleImports = [Import { importName = "Data.Text.Internal.Text"
                                                 , importAlias = Just "Internal"
                                                 , importQualified = True
                                                 , importItems = [ImportSingle "Text"]
                                                 }
                                         , Import { importName = "Data.Maybe"
                                                 , importAlias = Nothing
                                                 , importQualified = False
                                                 , importItems = []
                                                 }]
                       , moduleMetadata = [("package", "text")]
                       , moduleDecls = []
                       }
      show (printModule mod) `shouldBe` "---\npackage: text\n---\nmodule Data.Text\n  (Text)\nimport qualified Data.Text.Internal.Text as Internal (Text)\nimport           Data.Maybe  ()"

{-# LANGUAGE OverloadedStrings #-}

module PrintTest
  ( test
  )
where

import           Test.Hspec
import Print
import           Syntax
import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc (Doc)

import Prelude hiding (mod)

printMaybe :: Maybe (Doc a) -> Doc a
printMaybe = fromMaybe mempty

test :: Spec
test = do
  describe "printing types" $ do
    it "prints simple types" $ do
      show (printType (TyArr (TyVar "a") (TyVar "b"))) `shouldBe` "a -> b"
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
      let exports = ["fun1", "fun2", "SomeType"]
      show (printMaybe (printModExports exports)) `shouldBe` "(fun1, fun2, SomeType)"
    it "prints an empty module correctly" $ do
      let mod = Module { moduleName = "Data.Text"
                       , moduleExports = ["Text"]
                       , moduleImports = [Import { importName = "Data.Text.Internal.Text"
                                                 , importAlias = Just "Internal"
                                                 , importQualified = True
                                                 , importItems = ["Text"]
                                                 }
                                         , Import { importName = "Data.Maybe"
                                                 , importAlias = Nothing
                                                 , importQualified = False
                                                 , importItems = []
                                                 }]
                       , moduleMetadata = [("package", "text")]
                       , moduleDecls = []
                       }
      show (printModule mod) `shouldBe` "---\npackage: text\n---\nmodule Data.Text\n  (Text)\n\nimport qualified Data.Text.Internal.Text as Internal (Text)\nimport           Data.Maybe  ()\n"

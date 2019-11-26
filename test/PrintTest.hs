{-# LANGUAGE OverloadedStrings #-}

module PrintTest
  ( test
  )
where

import           Test.Hspec
import Print
import           Syntax

test :: Spec
test = do
  describe "printing module" $ do
    it "prints module metadata correctly" $ do
      let meta = [("hi", "there"), ("how", "are you")]
       in show (printMetadata meta) `shouldBe` "---\nhi: there\nhow: are you\n---"
    it "prints module name correctly" $ do
      let name = ModuleName ["Data", "List", "NonEmpty"]
      show (printModName name) `shouldBe` "module Data.List.NonEmpty"
    it "prints module exports correctly" $ do
      let exports = ["fun1", "fun2", "SomeType"]
      show (printModExports exports) `shouldBe` "(fun1, fun2, SomeType)"
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
      show (printModule mod) `shouldBe` "---\npackage: text\n---\nmodule Data.Text\n  (Text)\n\n\nimport qualified Data.Text.Internal.Text as Internal (Text)\nimport           Data.Maybe  ()\n\n\n"

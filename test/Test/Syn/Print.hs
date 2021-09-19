{-# LANGUAGE OverloadedLists #-}
module Test.Syn.Print
  ( test
  ) where

import           AST
import           Data.Maybe                     ( fromMaybe )
import           Data.Text.Prettyprint.Doc      ( Doc )
import           Syn
import           Syn.Print
import           Test.Hspec

import           Prelude                 hiding ( mod )

printMaybe :: Maybe (Doc a) -> Doc a
printMaybe = fromMaybe mempty

test :: Spec
test = parallel $ do
  describe "printing types" $ do
    it "prints simple types" $ do
      show (printType (TyVar "a" `fn` TyVar "b")) `shouldBe` "a -> b"
    it "prints type applications" $ do
      show (printType (TyCon "f" `tyapp` TyVar "a" `tyapp` TyVar "b"))
        `shouldBe` "f a b"
    it "prints nested type applications" $ do
      show (printType (TyCon "t" `tyapp` (TyCon "m" `tyapp` TyVar "a")))
        `shouldBe` "t (m a)"
      show (printType (TyCon "Reader" `tyapp` TyVar "m" `tyapp` TyVar "a"))
        `shouldBe` "Reader m a"
      show
          (printType
            (TyCon "a" `tyapp` (TyCon "b" `tyapp` (TyCon "c" `tyapp` TyVar "d"))
            )
          )
        `shouldBe` "a (b (c d))"
    it "prints type applications with arrows (1)" $ do
      show (printType (TyVar "a" `fn` TyCon "f" `tyapp` TyVar "a"))
        `shouldBe` "a -> f a"
    it "prints type applications with arrows (2)" $ do
      show
          (printType
            (TyCon "A" `tyapp` TyVar "a" `fn` TyCon "f" `tyapp` TyVar "a")
          )
        `shouldBe` "A a -> f a"
    it "prints type applications with arrows (3)" $ do
      show (printType ((TyCon "A" `fn` TyCon "A") `fn` TyCon "A"))
        `shouldBe` "(A -> A) -> A"
    it "prints tuple types" $ do
      show (printType (TyTuple [TyVar "a", TyVar "b"])) `shouldBe` "(a, b)"
    it "prints large tuple types on multiple lines" $ do
      let ty = TyTuple
            [ TyVar "AVeryLongType"
            , TyVar "AVeryLongType"
            , TyVar "AVeryLongType"
            , TyVar "AVeryLongType"
            , TyVar "AVeryLongType"
            , TyVar "AVeryLongType"
            ]
      show (printType ty)
        `shouldBe` "( AVeryLongType\n, AVeryLongType\n, AVeryLongType\n, AVeryLongType\n, AVeryLongType\n, AVeryLongType )"
  describe "printing string literals" $ do
    it "prints simple strings" $ do
      show (printInterpolatedString "hello" []) `shouldBe` "\"hello\""
    it "prints strings with escaped double quotes" $ do
      show (printInterpolatedString "hello quote: \"" [])
        `shouldBe` "\"hello quote: \\\"\""
    it "prints a string with an escaped backslash" $ do
      show (printInterpolatedString "hello backslash: \\" [])
        `shouldBe` "\"hello backslash: \\\\\""
    it "prints a string with an interpolation" $ do
      show (printInterpolatedString "hello " [(Var "name", "")])
        `shouldBe` "\"hello #{name}\""
    it "prints a string with more complex interpolation" $ do
      show
          (printInterpolatedString
            "hello "
            [(App (App (Var "+") (Var "name")) (StringLit "!"), "")]
          )
        `shouldBe` "\"hello #{(name + \"!\")}\""
    it "prints a string with a lone hash" $ do
      show (printInterpolatedString "hello hash: #" [])
        `shouldBe` "\"hello hash: #\""
    it "prints a string with an escaped hash bracket" $ do
      show (printInterpolatedString "hello hash bracket: #{" [])
        `shouldBe` "\"hello hash bracket: #\\{\""
    it "prints a string with several escaped backslashes" $ do
      show (printInterpolatedString "\\\\" []) `shouldBe` "\"\\\\\\\\\""
  describe "printing expressions" $ do
    it "prints cons expressions" $ do
      show (printExpr (App (App (Var "::") (Var "x")) (Var "y")))
        `shouldBe` "x :: y"
  describe "printing modules" $ do
    it "prints module name correctly" $ do
      let name = ModuleName ["Data", "List", "NonEmpty"]
      show (printModName name) `shouldBe` "module Data.List.NonEmpty"
    it "prints module exports correctly" $ do
      let exports =
            [("fun1", []), ("SomeType", []), ("OtherType", ["SomeConstructor"])]
      show (printMaybe (printModExports exports))
        `shouldBe` "{fun1, SomeType, OtherType{SomeConstructor}}"
    it "prints an empty module correctly" $ do
      let mod = Module
            { moduleName     = "text.Data.Text"
            , moduleExports  = [("Text", [])]
            , moduleImports  = [ Import
                                 { importName = "text.Data.Text.Internal.Text"
                                 , importAlias     = Just "Internal"
                                 , importQualified = True
                                 , importItems     = [ImportSingle "Text"]
                                 }
                               , Import { importName      = "std.Data.Maybe"
                                        , importAlias     = Nothing
                                        , importQualified = False
                                        , importItems     = []
                                        }
                               ]
            , moduleMetadata = []
            , moduleDecls    = []
            }
      show (printModule mod)
        `shouldBe` "module Data.Text\n  {Text}\n\nimport qualified Data.Text.Internal.Text as Internal {Text}\nfrom std import           Data.Maybe"
  describe "printing type definitions" $ do
    it "prints simple types" $ do
      show
          (printData Data
            { dataName   = "Maybe"
            , dataTyVars = ["a"]
            , dataCons   = [ DataCon { conName = "Nothing", conArgs = [] }
                           , DataCon { conName = "Just", conArgs = [TyVar "a"] }
                           ]
            }
          )
        `shouldBe` "type Maybe a {\n  Nothing,\n  Just a\n}"
    it "prints types with embedded records" $ do
      show
          (printData Data
            { dataName   = "Eq"
            , dataTyVars = ["a"]
            , dataCons   =
              [ DataCon
                  { conName = "Eq"
                  , conArgs =
                    [ TyRecord
                        [("eq", TyFun (TyVar "a") (TyFun (TyVar "a") TyBool))]
                    ]
                  }
              ]
            }
          )
        `shouldBe` "type Eq a {\n  Eq [eq: a -> a -> Bool]\n}"
    it "prints types no constructors" $ do
      show
          (printData Data { dataName = "Void", dataTyVars = [], dataCons = [] })
        `shouldBe` "type Void { }"
    it "prints types with no parameters" $ do
      show
          (printData Data
            { dataName   = "Bool"
            , dataTyVars = []
            , dataCons   = [ DataCon { conName = "True", conArgs = [] }
                           , DataCon { conName = "False", conArgs = [] }
                           ]
            }
          )
        `shouldBe` "type Bool {\n  True,\n  False\n}"
    it "prints types with multiple parameters" $ do
      show
          (printData Data
            { dataName = "Either"
            , dataTyVars = ["a", "b"]
            , dataCons = [ DataCon { conName = "Left", conArgs = [TyVar "a"] }
                         , DataCon { conName = "Right", conArgs = [TyVar "b"] }
                         ]
            }
          )
        `shouldBe` "type Either a b {\n  Left a,\n  Right b\n}"

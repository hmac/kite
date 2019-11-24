module ParseTest
  ( test
  )
where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec                ( parse )
import           Parse                          ( pModule
                                                , pDecl
                                                )

import           Syntax

test :: Spec
test = do
  describe "declarations" $ do
    it "parses a basic function definition" $ do
      parse pDecl "" "id : a -> a\nid x = x" `shouldParse` Decl
        { declName = Name "id"
        , declType = TyArr (TyVar (Name "a")) (TyVar (Name "a"))
        , declDefs = [ Def { defArgs = [VarPat (Name "x")]
                           , defExpr = Var (Name "x")
                           }
                     ]
        }

    it "parses a multiline function definition" $ do
      parse
          pDecl
          ""
          "head : [a] -> a\nhead [] = error \"head: empty list\"\nhead (Cons x xs) = x"
        `shouldParse` Decl
                        { declName = Name "head"
                        , declType = TyArr (TyList (TyVar (Name "a")))
                                           (TyVar (Name "a"))
                        , declDefs =
                          [ Def
                            { defArgs = [ListPat []]
                            , defExpr = App
                                          (Var (Name "error"))
                                          (Lit (LitString "head: empty list"))
                            }
                          , Def
                            { defArgs =
                              [ ConsPat
                                  (Name "Cons")
                                  [VarPat (Name "x"), VarPat (Name "xs")]
                              ]
                            , defExpr = Var (Name "x")
                            }
                          ]
                        }
  describe "modules" $ do
    it "parses a basic module" $ do
      parse pModule "" "module Foo ()\none : Int\none = 1" `shouldParse` Module
        { moduleName    = "Foo"
        , moduleImports = []
        , moduleExports = []
        , moduleDecls   =
          [ Decl { declName = Name "one"
                 , declType = TyCon (Name "Int") []
                 , declDefs = [Def { defArgs = [], defExpr = Lit (LitInt 1) }]
                 }
          ]
        }
    it "parses a module with imports and exports" $ do
      parse
          pModule
          ""
          "module Foo (fun1, fun2)\nimport Bar\nimport qualified Bar.Baz as B (fun3, fun4)"
        `shouldParse` Module
                        { moduleName    = "Foo"
                        , moduleImports =
                          [ Import { importQualified = False
                                   , importName      = ModuleName ["Bar"]
                                   , importAlias     = Nothing
                                   , importItems     = []
                                   }
                          , Import { importQualified = True
                                   , importName      = ModuleName ["Bar", "Baz"]
                                   , importAlias     = Just (Name "B")
                                   , importItems = [Name "fun3", Name "fun4"]
                                   }
                          ]
                        , moduleExports = [Name "fun1", Name "fun2"]
                        , moduleDecls   = []
                        }

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
      parse pDecl "" "id : a -> a\nid x = x" `shouldParse` FunDecl Fun
        { funName = Name "id"
        , funType = TyArr (TyVar (Name "a")) (TyVar (Name "a"))
        , funDefs = [ Def { defArgs = [VarPat (Name "x")]
                          , defExpr = Var (Name "x")
                          }
                    ]
        }

    it "parses a definition with multiple type arrows" $ do
      parse pDecl "" "const : a -> b -> a\nconst x y = x" `shouldParse` FunDecl
        Fun
          { funName = Name "const"
          , funType = TyArr (TyVar (Name "a"))
                            (TyArr (TyVar (Name "b")) (TyVar (Name "a")))
          , funDefs = [ Def { defArgs = [VarPat (Name "x"), VarPat (Name "y")]
                            , defExpr = Var (Name "x")
                            }
                      ]
          }
    it "parses a higher kinded type definition" $ do
      parse pDecl "" "map : (a -> b) -> f a -> f b\nmap f m = undefined"
        `shouldParse` FunDecl Fun
                        { funName = Name "map"
                        , funType = TyArr
                                      (TyArr (TyVar (Name "a"))
                                             (TyVar (Name "b"))
                                      )
                                      (TyArr
                                        (TyApp (Name "f") [TyVar (Name "a")])
                                        (TyApp (Name "f") [TyVar (Name "b")])
                                      )
                        , funDefs = [ Def
                                        { defArgs = [ VarPat (Name "f")
                                                    , VarPat (Name "m")
                                                    ]
                                        , defExpr = Var (Name "undefined")
                                        }
                                    ]
                        }
    it "parses a multiline function definition" $ do
      parse
          pDecl
          ""
          "head : [a] -> a\nhead [] = error \"head: empty list\"\nhead (Cons x xs) = x"
        `shouldParse` FunDecl Fun
                        { funName = Name "head"
                        , funType = TyArr (TyList (TyVar (Name "a")))
                                          (TyVar (Name "a"))
                        , funDefs =
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
    it "parses a simple data definition" $ do
      parse pDecl "" "data Unit = Unit" `shouldParse` DataDecl Data
        { dataName   = Name "Unit"
        , dataTyVars = []
        , dataCons   = [DataCon { conName = Name "Unit", conArgs = [] }]
        }
    it "parses the definition of List" $ do
      parse pDecl "" "data List a = Nil | Cons a (List a)"
        `shouldParse` DataDecl Data
                        { dataName   = Name "List"
                        , dataTyVars = [Name "a"]
                        , dataCons   =
                          [ DataCon { conName = Name "Nil", conArgs = [] }
                          , DataCon
                            { conName = Name "Cons"
                            , conArgs = [ TyVar (Name "a")
                                        , TyApp (Name "List") [TyVar (Name "a")]
                                        ]
                            }
                          ]
                        }
    it "parses a simple typeclass definition" $ do
      parse pDecl "" "class Functor f\n  map : (a -> b) -> f a -> f b\n"
        `shouldParse` TypeclassDecl Typeclass
                        { typeclassName   = Name "Functor"
                        , typeclassTyVars = [Name "f"]
                        , typeclassDefs   = [ ( Name "map"
                                              , TyArr
                                                (TyArr (TyVar (Name "a"))
                                                       (TyVar (Name "b"))
                                                )
                                                (TyArr
                                                  (TyApp (Name "f")
                                                         [TyVar (Name "a")]
                                                  )
                                                  (TyApp (Name "f")
                                                         [TyVar (Name "b")]
                                                  )
                                                )
                                              )
                                            ]
                        }
  describe "modules" $ do
    it "parses a basic module with metadata" $ do
      parse pModule "" "---\nkey: val\n---\nmodule Foo\none : Int\none = 1"
        `shouldParse` Module
                        { moduleName     = "Foo"
                        , moduleImports  = []
                        , moduleExports  = []
                        , moduleDecls    =
                          [ FunDecl Fun
                              { funName = Name "one"
                              , funType = TyVar (Name "Int")
                              , funDefs = [ Def { defArgs = []
                                                , defExpr = Lit (LitInt 1)
                                                }
                                          ]
                              }
                          ]
                        , moduleMetadata = [("key", "val")]
                        }
    it "parses a module with imports and exports" $ do
      parse
          pModule
          ""
          "module Foo (fun1, fun2)\nimport Bar\nimport qualified Bar.Baz as B (fun3, fun4)"
        `shouldParse` Module
                        { moduleName     = "Foo"
                        , moduleImports  =
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
                        , moduleExports  = [Name "fun1", Name "fun2"]
                        , moduleDecls    = []
                        , moduleMetadata = []
                        }

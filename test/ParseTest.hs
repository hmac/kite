module ParseTest
  ( test
  )
where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec                ( parse )
import           Syn.Parse                      ( pModule
                                                , pDecl
                                                , pExpr
                                                , pType
                                                )

import           Syntax


test :: Spec
test = do
  describe "parsing declarations" $ do
    it "parses a basic function definition" $ do
      parse pDecl "" "-- a comment\nid : a -> a\nid x = x" `shouldParse` FunDecl
        Fun { funComments   = ["a comment"]
            , funName       = "id"
            , funType       = TyVar "a" `fn` TyVar "a"
            , funConstraint = Nothing
            , funDefs = [Def { defArgs = [VarPat "x"], defExpr = Var "x" }]
            }

    it "parses a definition with multiple type arrows" $ do
      parse pDecl "" "const : a -> b -> a\nconst x y = x" `shouldParse` FunDecl
        Fun
          { funComments   = []
          , funName       = "const"
          , funType       = TyVar "a" `fn` TyVar "b" `fn` TyVar "a"
          , funConstraint = Nothing
          , funDefs       = [ Def { defArgs = [VarPat "x", VarPat "y"]
                                  , defExpr = Var "x"
                                  }
                            ]
          }
    it "parses a higher kinded type definition" $ do
      parse pDecl "" "map : (a -> b) -> f a -> f b\nmap f m = undefined"
        `shouldParse` FunDecl Fun
                        { funComments   = []
                        , funName       = "map"
                        , funType       = (TyVar "a" `fn` TyVar "b")
                                          `fn` ((TyVar "f") :@: (TyVar "a"))
                                          `fn` ((TyVar "f") :@: (TyVar "b"))
                        , funConstraint = Nothing
                        , funDefs = [ Def { defArgs = [VarPat "f", VarPat "m"]
                                          , defExpr = Var "undefined"
                                          }
                                    ]
                        }
    it "parses a multiline function definition" $ do
      parse
          pDecl
          ""
          "head : [a] -> a\nhead [] = error \"head: empty list\"\nhead (Cons x xs) = x"
        `shouldParse` FunDecl Fun
                        { funComments   = []
                        , funName       = "head"
                        , funType       = TyList (TyVar "a") `fn` TyVar "a"
                        , funConstraint = Nothing
                        , funDefs       =
                          [ Def
                            { defArgs = [ListPat []]
                            , defExpr = App (Var "error")
                                            (StringLit "head: empty list" [])
                            }
                          , Def
                            { defArgs = [ ConsPat "Cons"
                                                  [VarPat "x", VarPat "xs"]
                                        ]
                            , defExpr = Var "x"
                            }
                          ]
                        }
    it "parses a function with typeclass constraints" $ do
      parse pDecl "" "concat : Monoid a => [a] -> a\nconcat [] = empty"
        `shouldParse` FunDecl Fun
                        { funComments   = []
                        , funName       = "concat"
                        , funType       = TyList (TyVar "a") `fn` TyVar "a"
                        , funConstraint = Just (CInst "Monoid" [TyVar "a"])
                        , funDefs       = [ Def { defArgs = [ListPat []]
                                                , defExpr = Var "empty"
                                                }
                                          ]
                        }
      parse pDecl "" "foo : (Eq a, Show a) => a -> String\nfoo x = bar"
        `shouldParse` FunDecl Fun
                        { funComments   = []
                        , funName       = "foo"
                        , funType       = (TyVar "a") `fn` TyCon "String"
                        , funConstraint =
                          Just
                            (CTuple (CInst "Eq" [TyVar "a"])
                                    (CInst "Show" [TyVar "a"])
                            )
                        , funDefs       = [ Def { defArgs = [VarPat "x"]
                                                , defExpr = Var "bar"
                                                }
                                          ]
                        }
    it "parses a function with a multi param argument type" $ do
      parse
          pDecl
          ""
          "fromLeft : Either a b -> Maybe a\nfromLeft (Left x) = Just x\nfromLeft (Right _) = Nothing"
        `shouldParse` FunDecl Fun
                        { funComments   = []
                        , funName       = "fromLeft"
                        , funType = (TyCon "Either" :@: TyVar "a" :@: TyVar "b")
                                      `fn` (TyCon "Maybe" :@: TyVar "a")
                        , funConstraint = Nothing
                        , funDefs       =
                          [ Def { defArgs = [ConsPat "Left" [VarPat "x"]]
                                , defExpr = App (Cons "Just") (Var "x")
                                }
                          , Def { defArgs = [ConsPat "Right" [WildPat]]
                                , defExpr = Cons "Nothing"
                                }
                          ]
                        }

    it "parses a simple data definition" $ do
      parse pDecl "" "data Unit = Unit" `shouldParse` DataDecl Data
        { dataName   = "Unit"
        , dataTyVars = []
        , dataCons   = [DataCon { conName = "Unit", conArgs = [] }]
        }
    it "parses the definition of List" $ do
      parse pDecl "" "data List a = Nil | Cons a (List a)"
        `shouldParse` DataDecl Data
                        { dataName = "List"
                        , dataTyVars = ["a"]
                        , dataCons = [ DataCon { conName = "Nil", conArgs = [] }
                                     , DataCon
                                       { conName = "Cons"
                                       , conArgs = [ TyVar "a"
                                                   , (TyCon "List")
                                                     :@: (TyVar "a")
                                                   ]
                                       }
                                     ]
                        }
    it "parses a simple typeclass definition" $ do
      parse pDecl "" "class Functor f where\n  map : (a -> b) -> f a -> f b\n"
        `shouldParse` TypeclassDecl Typeclass
                        { typeclassName   = "Functor"
                        , typeclassTyVars = ["f"]
                        , typeclassDefs   = [ ( "map"
                                              , (TyVar "a" `fn` TyVar "b")
                                              `fn` ((TyVar "f") :@: (TyVar "a"))
                                              `fn` ((TyVar "f") :@: (TyVar "b"))
                                              )
                                            ]
                        }
    it "parses a typeclass instance" $ do
      parse
          pDecl
          ""
          "instance Functor Maybe where\n  map _ Nothing = Nothing\n  map f (Just x) = Just (f x)\n"
        `shouldParse` TypeclassInst Instance
                        { instanceName  = "Functor"
                        , instanceTypes = [TyCon "Maybe"]
                        , instanceDefs  =
                          [ ( "map"
                            , [ Def { defArgs = [WildPat, ConsPat "Nothing" []]
                                    , defExpr = Cons "Nothing"
                                    }
                              , Def
                                { defArgs = [ VarPat "f"
                                            , ConsPat "Just" [VarPat "x"]
                                            ]
                                , defExpr = App (Cons "Just")
                                                (App (Var "f") (Var "x"))
                                }
                              ]
                            )
                          ]
                        }
  describe "parsing modules" $ do
    it "parses a basic module with metadata" $ do
      parse pModule "" "---\nkey: val\n---\nmodule Foo\none : Int\none = 1"
        `shouldParse` Module
                        { moduleName     = "Foo"
                        , moduleImports  = []
                        , moduleExports  = []
                        , moduleDecls    =
                          [ FunDecl Fun
                              { funName       = "one"
                              , funComments   = []
                              , funType       = TyCon "Int"
                              , funConstraint = Nothing
                              , funDefs       = [ Def { defArgs = []
                                                      , defExpr = IntLit 1
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
                                   , importAlias     = Just "B"
                                   , importItems     = ["fun3", "fun4"]
                                   }
                          ]
                        , moduleExports  = ["fun1", "fun2"]
                        , moduleDecls    = []
                        , moduleMetadata = []
                        }
  describe "parsing expressions" $ do
    it "parses an application" $ do
      parse pExpr "" "foo x y z"
        `shouldParse` App (App (App (Var "foo") (Var "x")) (Var "y")) (Var "z")
    it "parses a case expression" $ do
      parse pExpr "" "case x of\n  Just y -> y\n  Nothing -> z"
        `shouldParse` Case
                        (Var "x")
                        [ (ConsPat "Just" [VarPat "y"], Var "y")
                        , (ConsPat "Nothing" []       , Var "z")
                        ]
    it "parses a let expression" $ do
      parse pExpr "" "let x = 1\n    y = 2\n in add x y" `shouldParse` Let
        [("x", IntLit 1), ("y", IntLit 2)]
        (App (App (Var "add") (Var "x")) (Var "y"))
    it "parses a binary operator" $ do
      parse pExpr "" "1 + 1"
        `shouldParse` App (App (Var "+") (IntLit 1)) (IntLit 1)
    it "parses a tuple" $ do
      parse pExpr "" "(\"\", 0.0)"
        `shouldParse` TupleLit [StringLit "" [], FloatLit 0.0]
  describe "parsing types" $ do
    it "parses basic function types" $ do
      parse pType "" "a -> b" `shouldParse` (TyVar "a" `fn` TyVar "b")
    it "parses multi arg function types" $ do
      parse pType "" "a -> b -> c"
        `shouldParse` (TyVar "a" `fn` TyVar "b" `fn` TyVar "c")
    it "parses higher order function types" $ do
      parse pType "" "(a -> b) -> a -> b"
        `shouldParse` ((TyVar "a" `fn` TyVar "b") `fn` TyVar "a" `fn` TyVar "b")
    it "parses multi parameter type constructors" $ do
      parse pType "" "Either a b -> Maybe a"
        `shouldParse` (    (TyCon "Either" :@: TyVar "a" :@: TyVar "b")
                      `fn` (TyCon "Maybe" :@: TyVar "a")
                      )

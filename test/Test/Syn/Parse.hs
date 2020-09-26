module Test.Syn.Parse
  ( test
  )
where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec                ( parse
                                                , eof
                                                )
import           Syn.Parse                      ( pModule
                                                , pDecl
                                                , pExpr
                                                , pType
                                                , pPattern
                                                )

import           AST
import           Syn

-- Parse the string as an expression
-- TODO: roll out to this whole module
parseExpr str = parse (pExpr <* eof) "" str

test :: Spec
test = parallel $ do
  describe "parsing declarations" $ do
    it "parses a basic function definition" $ do
      parse pDecl "" "id : a -> a\nid = x -> x" `shouldParse` FunDecl Fun
        { funComments = []
        , funName     = "id"
        , funType     = Just (TyVar "a" `fn` TyVar "a")
        , funExpr     = MCase [([VarPat "x"], Var "x")]
        }
    it "requires a line fold to be indented" $ do
      parse pDecl "" `shouldFailOn` "id : a -> a\nid x =\nx"

    it "parses a definition with multiple type arrows" $ do
      parse pDecl "" "const : a -> b -> a\nconst = x y -> x"
        `shouldParse` FunDecl Fun
                        { funComments = []
                        , funName = "const"
                        , funType = Just $ TyVar "a" `fn` TyVar "b" `fn` TyVar
                                      "a"
                        , funExpr = MCase [([VarPat "x", VarPat "y"], Var "x")]
                        }
    it "parses a higher kinded type definition" $ do
      parse pDecl "" "map : (a -> b) -> f a -> f b\nmap = f m -> undefined"
        `shouldParse` FunDecl Fun
                        { funComments = []
                        , funName     = "map"
                        , funType     = Just
                                        $    (TyVar "a" `fn` TyVar "b")
                                        `fn` TyApp (TyVar "f") (TyVar "a")
                                        `fn` TyApp (TyVar "f") (TyVar "b")
                        , funExpr     = MCase
                          [([VarPat "f", VarPat "m"], Var "undefined")]
                        }
    it "parses a multiline function definition" $ do
      parse
          pDecl
          ""
          "head : [a] -> a\nhead = [] -> error \"head: empty list\"\n       (Cons x xs) -> x"
        `shouldParse` FunDecl Fun
                        { funComments = []
                        , funName     = "head"
                        , funType = Just $ TyApp TyList (TyVar "a") `fn` TyVar
                                      "a"
                        , funExpr     = MCase
                          [ ( [ListPat []]
                            , App (Var "error") (StringLit "head: empty list")
                            )
                          , ( [ConsPat "Cons" [VarPat "x", VarPat "xs"]]
                            , Var "x"
                            )
                          ]
                        }
    it "parses a function with a multi param argument type" $ do
      parse
          pDecl
          ""
          "fromLeft : Either a b -> Maybe a\nfromLeft = (Left x) -> Just x\n           (Right _) -> Nothing"
        `shouldParse` FunDecl Fun
                        { funComments = []
                        , funName     = "fromLeft"
                        , funType     =
                          Just
                          $    TyApp (TyApp (TyCon "Either") (TyVar "a"))
                                     (TyVar "b")
                          `fn` TyApp (TyCon "Maybe") (TyVar "a")
                        , funExpr     = MCase
                                          [ ( [ConsPat "Left" [VarPat "x"]]
                                            , App (Con "Just") (Var "x")
                                            )
                                          , ( [ConsPat "Right" [WildPat]]
                                            , Con "Nothing"
                                            )
                                          ]
                        }

    it "parses a simple type definition" $ do
      parse pDecl "" "type Unit = Unit" `shouldParse` DataDecl Data
        { dataName   = "Unit"
        , dataTyVars = []
        , dataCons   = [DataCon { conName = "Unit", conArgs = [] }]
        }
    it "parses a record definition" $ do
      parse (pDecl <* eof)
            ""
            "type Foo a = Foo { unFoo : a, label : ?b, c : A A }"
        `shouldParse` DataDecl Data
                        { dataName   = "Foo"
                        , dataTyVars = ["a"]
                        , dataCons   =
                          [ DataCon
                              { conName = "Foo"
                              , conArgs =
                                [ TyRecord
                                    [ ("unFoo", TyVar "a")
                                    , ("label", TyHole "b")
                                    , ("c"    , TyCon "A" `tyapp` TyCon "A")
                                    ]
                                ]
                              }
                          ]
                        }
    it "parses the definition of List" $ do
      parse pDecl "" "type List a = Nil | Cons a (List a)"
        `shouldParse` DataDecl Data
                        { dataName = "List"
                        , dataTyVars = ["a"]
                        , dataCons = [ DataCon { conName = "Nil", conArgs = [] }
                                     , DataCon
                                       { conName = "Cons"
                                       , conArgs = [ TyVar "a"
                                                   , TyCon "List"
                                                     `tyapp` TyVar "a"
                                                   ]
                                       }
                                     ]
                        }
    it "parses a simple type alias definition" $ do
      parse pDecl "" "type alias Foo = Int" `shouldParse` AliasDecl Alias
        { aliasName   = "Foo"
        , aliasTyVars = []
        , aliasType   = TyInt
        }
    it "parses a type alias definition with type variables" $ do
      parse pDecl "" "type alias MyList a = [a]" `shouldParse` AliasDecl Alias
        { aliasName   = "MyList"
        , aliasTyVars = ["a"]
        , aliasType   = TyApp TyList (TyVar "a")
        }
  describe "parsing modules" $ do
    it "parses a basic module with metadata" $ do
      parse pModule "" "---\nkey: val\n---\nmodule Foo\none : Int\none = 1"
        `shouldParse` Module
                        { moduleName     = "Foo"
                        , moduleImports  = []
                        , moduleExports  = []
                        , moduleDecls    = [ FunDecl Fun { funName = "one"
                                                         , funComments = []
                                                         , funType = Just TyInt
                                                         , funExpr = IntLit 1
                                                         }
                                           ]
                        , moduleMetadata = [("key", "val")]
                        }
    it "parses a module with imports and exports" $ do
      parse
          pModule
          ""
          "module Foo (fun1, fun2)\nimport Bar\nimport qualified Bar.Baz as B (fun3, fun4, Foo(..), Bar(BarA, BarB))"
        `shouldParse` Module
                        { moduleName     = "Foo"
                        , moduleImports  =
                          [ Import { importQualified = False
                                   , importName      = ModuleName ["Bar"]
                                   , importAlias     = Nothing
                                   , importItems     = []
                                   }
                          , Import
                            { importQualified = True
                            , importName      = ModuleName ["Bar", "Baz"]
                            , importAlias     = Just "B"
                            , importItems = [ ImportSingle "fun3"
                                            , ImportSingle "fun4"
                                            , ImportAll "Foo"
                                            , ImportSome "Bar" ["BarA", "BarB"]
                                            ]
                            }
                          ]
                        , moduleExports  = [("fun1", []), ("fun2", [])]
                        , moduleDecls    = []
                        , moduleMetadata = []
                        }
  describe "parsing patterns" $ do
    it "parses an int pattern" $ do
      parse pPattern "" "1" `shouldParse` IntPat 1
    it "parses a wildcard pattern" $ do
      parse pPattern "" "_" `shouldParse` WildPat
    it "parses a char pattern" $ do
      parse pPattern "" "'c'" `shouldParse` CharPat 'c'
    it "parses a list pattern" $ do
      parse pPattern "" "[1,_,3]"
        `shouldParse` ListPat [IntPat 1, WildPat, IntPat 3]
    it "parses a unit pattern" $ do
      parse pPattern "" "()" `shouldParse` UnitPat
    it "parses a tuple pattern" $ do
      parse pPattern "" "(1, _)" `shouldParse` TuplePat [IntPat 1, WildPat]
    it "parses a variable pattern" $ do
      parse pPattern "" "foo" `shouldParse` VarPat "foo"
  describe "parsing expressions" $ do
    it "parses an application" $ do
      parse pExpr "" "foo x y z"
        `shouldParse` App (App (App (Var "foo") (Var "x")) (Var "y")) (Var "z")
    it "parses an infix application" $ do
      parse pExpr "" "(a <= a)"
        `shouldParse` App (App (Var "<=") (Var "a")) (Var "a")
    it "parses a case expression" $ do
      parse pExpr "" "case x of\n  Just y -> y\n  Nothing -> z"
        `shouldParse` Case
                        (Var "x")
                        [ (ConsPat "Just" [VarPat "y"], Var "y")
                        , (ConsPat "Nothing" []       , Var "z")
                        ]
    it "parses a case with variable patterns" $ do
      parse pExpr "" "case x of\n  y -> y"
        `shouldParse` Case (Var "x") [(VarPat "y", Var "y")]
    it "parses a let expression" $ do
      parse pExpr "" "let x = 1\n    y = 2\n in add x y" `shouldParse` Let
        [("x", IntLit 1), ("y", IntLit 2)]
        (App (App (Var "add") (Var "x")) (Var "y"))
    it "parses a binary operator" $ do
      parse pExpr "" "1 + 1"
        `shouldParse` App (App (Var "+") (IntLit 1)) (IntLit 1)
    it "parses a tuple" $ do
      parse pExpr "" "(\"\", 0)" `shouldParse` TupleLit [StringLit "", IntLit 0]
    it "parses a record" $ do
      parse pExpr "" "{ a = a, b = b }"
        `shouldParse` Record [("a", Var "a"), ("b", Var "b")]
    it "parses record projection" $ do
      parseExpr "a.b" `shouldParse` Project (Var "a") "b"
      parse pExpr "" "f a.b" `shouldParse` App (Var "f") (Project (Var "a") "b")
      -- Not currently supported
      -- parseExpr "{ a = a }.b"
      --   `shouldParse` (Project (Record [("a", Var "a")]) "b")
  describe "parsing string literals" $ do
    it "parses a simple string" $ do
      parse pExpr "" "\"hello\"" `shouldParse` StringLit "hello"
    it "parses a string with escaped double quotes" $ do
      parse pExpr "" "\"hello quote: \\\"\""
        `shouldParse` StringLit "hello quote: \""
    it "parses a string with an escaped backslash" $ do
      parse pExpr "" "\"hello backslash: \\\\\""
        `shouldParse` StringLit "hello backslash: \\"
    it "parses a string with an escaped newline" $ do
      parse pExpr "" "\"hello newline: \\n\""
        `shouldParse` StringLit "hello newline: \n"
    it "parses a string with an interpolation" $ do
      parse pExpr "" "\"hello #{name}\""
        `shouldParse` StringInterp "hello " [(Var "name", "")]
    it "parses a string with more complex interpolation" $ do
      parse pExpr "" "\"hello #{name + \"!\"}\"" `shouldParse` StringInterp
        "hello "
        [(App (App (Var "+") (Var "name")) (StringLit "!"), "")]
    it "parses a string with a lone hash" $ do
      parse pExpr "" "\"hello hash: #\"" `shouldParse` StringLit "hello hash: #"
    it "parses a string with an escaped hash bracket" $ do
      parse pExpr "" "\"hello hash bracket: #\\{\""
        `shouldParse` StringLit "hello hash bracket: #{"
    it "parses a string with several escaped backslashes" $ do
      parse pExpr "" "\"\\\\\\\\\"" `shouldParse` StringLit "\\\\"
  describe "parsing types" $ do
    it "basic applications" $ do
      parse pType "" "f a" `shouldParse` (TyVar "f" `tyapp` TyVar "a")
    it "applications on the left of applications" $ do
      parse pType "" "f a b"
        `shouldParse` (TyVar "f" `tyapp` TyVar "a" `tyapp` TyVar "b")
    it "applications on the right of applications" $ do
      parse pType "" "f (m a)"
        `shouldParse` (TyVar "f" `tyapp` (TyVar "m" `tyapp` TyVar "a"))
    it "applications on the left of arrows" $ do
      parse pType "" "f a -> b"
        `shouldParse` (TyVar "f" `tyapp` TyVar "a" `fn` TyVar "b")
    it "applications on the right of arrows" $ do
      parse pType "" "a -> f a"
        `shouldParse` (TyVar "a" `fn` TyVar "f" `tyapp` TyVar "a")
    -- This is not well-typed but should still parse
    it "arrows on the left of applications" $ do
      parse pType "" "(a -> b) f"
        `shouldParse` ((TyVar "a" `fn` TyVar "b") `tyapp` TyVar "f")
    it "arrows on the right of applications" $ do
      parse pType "" "f (a -> b)"
        `shouldParse` (TyVar "f" `tyapp` (TyVar "a" `fn` TyVar "b"))
    it "arrows on the left of arrows" $ do
      parse pType "" "(a -> b) -> c"
        `shouldParse` ((TyVar "a" `fn` TyVar "b") `fn` TyVar "c")
    it "arrows on the right of arrows" $ do
      parse pType "" "a -> b -> c"
        `shouldParse` (TyVar "a" `fn` TyVar "b" `fn` TyVar "c")
    it "arrows on the right of arrows (with parens)" $ do
      parse pType "" "a -> (b -> c)"
        `shouldParse` (TyVar "a" `fn` TyVar "b" `fn` TyVar "c")
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
        `shouldParse` (       TyCon "Either"
                      `tyapp` TyVar "a"
                      `tyapp` TyVar "b"
                      `fn`    TyCon "Maybe"
                      `tyapp` TyVar "a"
                      )
    it "parses parameterised constructors inside lists" $ do
      parse pType "" "[Maybe a]"
        `shouldParse` TyApp TyList (TyCon "Maybe" `tyapp` TyVar "a")
    it "parses function types in lists" $ do
      parse pType "" "[a -> b]"
        `shouldParse` TyApp TyList (TyVar "a" `fn` TyVar "b")
    it "parses function types in higher kinded types" $ do
      parse pType "" "A [a -> a]"
        `shouldParse` TyApp
                        (TyCon "A")
                        (TyApp TyList (TyVar "a" `fn` TyVar "a"))
      parse pType "" "A [B -> C]"
        `shouldParse` TyApp
                        (TyCon "A")
                        (TyApp TyList (TyCon "B" `fn` TyCon "C"))
    it "parses nested type constructors" $ do
      parse pType "" "A B" `shouldParse` TyApp (TyCon "A") (TyCon "B")
    it "parses record types" $ do
      parse pType "" "{x : a, y : b}"
        `shouldParse` TyRecord [("x", TyVar "a"), ("y", TyVar "b")]
    it "parses applications of holes to types" $ do
      parse pType "" "?a A" `shouldParse` (TyHole "a" `tyapp` TyCon "A")
    it "parses quantification" $ do
      parse pType "" "forall a. a -> a"
        `shouldParse` TyForall "a" (TyVar "a" `fn` TyVar "a")
    it "parses multi quantification" $ do
      parse pType "" "forall a b. a -> b"
        `shouldParse` TyForall "a" (TyForall "b" (TyVar "a" `fn` TyVar "b"))
    it "parses complex multi quantification" $ do
      parse pType "" "forall a b. (a -> b) -> f a -> f b"
        `shouldParse` (TyForall
                        "a"
                        (TyForall
                          "b"
                          (    (TyVar "a" `fn` TyVar "b")
                          `fn` (TyVar "f" `tyapp` TyVar "a")
                          `fn` (TyVar "f" `tyapp` TyVar "b")
                          )
                        )
                      )

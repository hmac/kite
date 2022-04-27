{-# LANGUAGE OverloadedLists #-}
module Test.Syn.Parse
  ( test
  ) where

import           Syn.Parse                      ( Error
                                                , pDecl
                                                , pExpr
                                                , pModule
                                                , pType
                                                )
import           Syn.Parse.Common               ( parse' )
import           Syn.Parse.Pattern              ( pPattern )
import           Test.Hspec
import           Test.Hspec.Megaparsec          ( shouldFailOn
                                                , shouldParse
                                                )
import           Text.Megaparsec                ( ParseErrorBundle
                                                , eof
                                                )

import           AST
import           Syn

-- Parse the string as an expression
-- TODO: roll out to this whole module
parseExpr :: String -> Either (ParseErrorBundle String Error) Syn
parseExpr = parse' (pExpr <* eof) ""

test :: Spec
test = parallel $ do
  describe "parsing declarations" $ do
    it "parses a basic function definition" $ do
      parse' pDecl "" "id : a -> a\nid = x -> x" `shouldParse` FunDecl Fun
        { funComments = []
        , funName     = "id"
        , funType     = Just (TyVar "a" `fn` TyVar "a")
        , funExpr     = MCase [([VarPat "x"], Var "x")]
        , funWheres   = []
        }
    it "requires a line fold to be indented" $ do
      parse' pDecl "" `shouldFailOn` "id : a -> a\nid x =\nx"

    it "parses a definition with multiple type arrows" $ do
      parse' pDecl "" "const : a -> b -> a\nconst = x y -> x"
        `shouldParse` FunDecl Fun
                        { funComments = []
                        , funName = "const"
                        , funType = Just $ TyVar "a" `fn` TyVar "b" `fn` TyVar
                                      "a"
                        , funExpr = MCase [([VarPat "x", VarPat "y"], Var "x")]
                        , funWheres = []
                        }
    it "parses a higher kinded type definition" $ do
      parse' pDecl "" "map : (a -> b) -> f a -> f b\nmap = f m -> undefined"
        `shouldParse` FunDecl Fun
                        { funComments = []
                        , funName     = "map"
                        , funType     = Just
                                        $    (TyVar "a" `fn` TyVar "b")
                                        `fn` TyApp (TyVar "f") (TyVar "a")
                                        `fn` TyApp (TyVar "f") (TyVar "b")
                        , funExpr     = MCase
                          [([VarPat "f", VarPat "m"], Var "undefined")]
                        , funWheres   = []
                        }
    it "parses a multiline function definition" $ do
      parse'
          (pDecl <* eof)
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
                          , ( [ConsPat "Cons" Nothing [VarPat "x", VarPat "xs"]]
                            , Var "x"
                            )
                          ]
                        , funWheres   = []
                        }
    it "parses a function with a multi param argument type" $ do
      parse'
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
                                          [ ( [ConsPat "Left" Nothing [VarPat "x"]]
                                            , App (Con "Just") (Var "x")
                                            )
                                          , ( [ConsPat "Right" Nothing [WildPat]]
                                            , Con "Nothing"
                                            )
                                          ]
                        , funWheres   = []
                        }
    it "parses a case expression inside an mcase" $ do
      let
        str
          = "functor : Alternative f -> Functor f\nfunctor = f -> case applicative f of\n                 (Applicative g) -> g.functor"
      parse' pDecl "" str `shouldParse` FunDecl Fun
        { funComments = []
        , funName     = "functor"
        , funType = Just $ TyApp (TyCon "Alternative") (TyVar "f") `fn` TyApp
                      (TyCon "Functor")
                      (TyVar "f")
        , funExpr     = MCase
                          [ ( [VarPat "f"]
                            , Case
                              (App (Var "applicative") (Var "f"))
                              [ ( ConsPat "Applicative" Nothing [VarPat "g"]
                                , Project (Var "g") "functor"
                                )
                              ]
                            )
                          ]
        , funWheres   = []
        }

    it "parses a simple type definition" $ do
      parse' pDecl "" "type Unit = Unit" `shouldParse` DataDecl Data
        { dataName   = "Unit"
        , dataTyVars = []
        , dataCons   = [DataCon { conName = "Unit", conArgs = [] }]
        }
    it "parses a record definition" $ do
      parse' (pDecl <* eof)
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
      parse' pDecl "" "type List a = Nil | Cons a (List a)"
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
      parse' pDecl "" "type alias Foo = Int" `shouldParse` AliasDecl Alias
        { aliasName   = "Foo"
        , aliasTyVars = []
        , aliasType   = TyInt
        }
    it "parses a type alias definition with type variables" $ do
      parse' pDecl "" "type alias MyList a = [a]" `shouldParse` AliasDecl Alias
        { aliasName   = "MyList"
        , aliasTyVars = ["a"]
        , aliasType   = TyApp TyList (TyVar "a")
        }
  describe "parsing modules" $ do
    it "parses a basic module with metadata" $ do
      parse' (pModule "test")
             ""
             "---\nkey: val\n---\nmodule Foo\none : Int\none = 1"
        `shouldParse` Module
                        { moduleName     = "test.Foo"
                        , moduleImports  = []
                        , moduleExports  = []
                        , moduleDecls    = [ FunDecl Fun { funName = "one"
                                                         , funComments = []
                                                         , funType = Just TyInt
                                                         , funExpr = IntLit 1
                                                         , funWheres = []
                                                         }
                                           ]
                        , moduleMetadata = [("key", "val")]
                        }
    it "parses a module with imports and exports" $ do
      parse'
          (pModule "test")
          ""
          "module Foo (fun1, fun2)\nimport Bar\nimport qualified Bar.Baz as B (fun3, fun4, Foo(..), Bar(BarA, BarB))\nfrom somepkg import Http"
        `shouldParse` Module
                        { moduleName     = "test.Foo"
                        , moduleImports  =
                          [ Import { importQualified = False
                                   , importName      = "test.Bar"
                                   , importAlias     = Nothing
                                   , importItems     = []
                                   }
                          , Import
                            { importQualified = True
                            , importName      = "test.Bar.Baz"
                            , importAlias     = Just "B"
                            , importItems = [ ImportSingle "fun3"
                                            , ImportSingle "fun4"
                                            , ImportAll "Foo"
                                            , ImportSome "Bar" ["BarA", "BarB"]
                                            ]
                            }
                          , Import { importQualified = False
                                   , importName      = "somepkg.Http"
                                   , importAlias     = Nothing
                                   , importItems     = []
                                   }
                          ]
                        , moduleExports  = [("fun1", []), ("fun2", [])]
                        , moduleDecls    = []
                        , moduleMetadata = []
                        }
  describe "parsing patterns" $ do
    it "parses an int pattern" $ do
      parse' pPattern "" "1" `shouldParse` IntPat 1
    it "parses a wildcard pattern" $ do
      parse' pPattern "" "_" `shouldParse` WildPat
    it "parses a char pattern" $ do
      parse' pPattern "" "'c'" `shouldParse` CharPat 'c'
    it "parses a list pattern" $ do
      parse' pPattern "" "[1,_,3]"
        `shouldParse` ListPat [IntPat 1, WildPat, IntPat 3]
    it "parses a unit pattern" $ do
      parse' pPattern "" "()" `shouldParse` UnitPat
    it "parses a tuple pattern" $ do
      parse' pPattern "" "(1, _)" `shouldParse` TuplePat [IntPat 1, WildPat]
    it "parses a variable pattern" $ do
      parse' pPattern "" "foo" `shouldParse` VarPat "foo"
  describe "parsing expressions" $ do
    it "parses an application" $ do
      parse' pExpr "" "foo x y z"
        `shouldParse` App (App (App (Var "foo") (Var "x")) (Var "y")) (Var "z")
    it "parses an infix application" $ do
      parse' pExpr "" "(a <= a)"
        `shouldParse` App (App (Var "<=") (Var "a")) (Var "a")
    it "parses multiple infix applications" $ do
      parse' (pExpr <* eof) "" "1 + 2 + 3" `shouldParse` App
        (App (Var "+") (App (App (Var "+") (IntLit 1)) (IntLit 2)))
        (IntLit 3)
    it "parses multiple infix applications with different associativity" $ do
      parse' (pExpr <* eof) "" "3 - 2 - 1" `shouldParse` App
        (App (Var "-") (App (App (Var "-") (IntLit 3)) (IntLit 2)))
        (IntLit 1)
    it "parses function composition (1)" $ do
      parse' (pExpr <* eof) "" "f . g"
        `shouldParse` App (App (Var ".") (Var "f")) (Var "g")
    it "parses function composition (2)" $ do
      parse' (pExpr <* eof) "" "f . g . h" `shouldParse` App
        (App (Var ".") (Var "f"))
        (App (App (Var ".") (Var "g")) (Var "h"))
    it "parses function composition (3)" $ do
      parse' (pExpr <* eof) "" "f . g y z . h" `shouldParse` App
        (App (Var ".") (Var "f"))
        (App (App (Var ".") (App (App (Var "g") (Var "y")) (Var "z"))) (Var "h")
        )
    it "parses function composition (3)" $ do
      parse' (pExpr <* eof) "" "f . (x -> x) . h" `shouldParse` App
        (App (Var ".") (Var "f"))
        (App (App (Var ".") (MCase [([VarPat "x"], Var "x")])) (Var "h"))
    it "parses a case expression" $ do
      parse' pExpr "" "case x of\n  Just y -> y\n  Nothing -> z"
        `shouldParse` Case
                        (Var "x")
                        [ (ConsPat "Just" Nothing [VarPat "y"], Var "y")
                        , (ConsPat "Nothing" Nothing []       , Var "z")
                        ]
    it "parses a case with variable patterns" $ do
      parse' pExpr "" "case x of\n  y -> y"
        `shouldParse` Case (Var "x") [(VarPat "y", Var "y")]
    it "parses a let expression on one line" $ do
      parse' pExpr "" "let x = 1 in add x 1" `shouldParse` Let
        [("x", IntLit 1, Nothing)]
        (App (App (Var "add") (Var "x")) (IntLit 1))
    it "parses a let expression split over two lines" $ do
      parse' pExpr "" "let x = 1\n    y = 2\n in add x y" `shouldParse` Let
        [("x", IntLit 1, Nothing), ("y", IntLit 2, Nothing)]
        (App (App (Var "add") (Var "x")) (Var "y"))
    it "parses a let expression with a multiline binding" $ do
      -- let x = foo
      --          bar
      --     y = baz
      --  in x
      parseExpr "let x = foo\n         bar\n    y = baz\n in x"
        `shouldParse` Let
                        [ ("x", App (Var "foo") (Var "bar"), Nothing)
                        , ("y", Var "baz"                  , Nothing)
                        ]
                        (Var "x")
    it "parses an annotated let expression" $ do
      parseExpr "let f : Int -> Int\n    f = i -> i + 1\n in f 1"
        `shouldParse` Let
                        [ ( "f"
                          , MCase
                            [ ( [VarPat "i"]
                              , App (App (Var "+") (Var "i")) (IntLit 1)
                              )
                            ]
                          , Just (TyFun TyInt TyInt)
                          )
                        ]
                        (App (Var "f") (IntLit 1))
    it "parses multiple annotated let bindings" $ do
      parseExpr
          "let f : Int -> Int\n    f = i -> i + 1\n    g : Bool -> Int\n    g = True -> 1\n        False -> 0\n in 1"
        `shouldParse` Let
                        [ ( "f"
                          , MCase
                            [ ( [VarPat "i"]
                              , App (App (Var "+") (Var "i")) (IntLit 1)
                              )
                            ]
                          , Just (TyFun TyInt TyInt)
                          )
                        , ( "g"
                          , MCase
                            [ ([BoolPat True] , IntLit 1)
                            , ([BoolPat False], IntLit 0)
                            ]
                          , Just (TyFun TyBool TyInt)
                          )
                        ]
                        (IntLit 1)
    it "parses a binary operator" $ do
      parse' pExpr "" "1 + 1"
        `shouldParse` App (App (Var "+") (IntLit 1)) (IntLit 1)
    it "parses a tuple" $ do
      parse' pExpr "" "(\"\", 0)"
        `shouldParse` TupleLit [StringLit "", IntLit 0]
    it "parses a record" $ do
      parse' pExpr "" "{ a = a, b = b }"
        `shouldParse` Record [("a", Var "a"), ("b", Var "b")]
    it "parses record projection" $ do
      parseExpr "a.b" `shouldParse` Project (Var "a") "b"
      parse' pExpr "" "f a.b"
        `shouldParse` App (Var "f") (Project (Var "a") "b")
      -- Not currently supported
      -- parseExpr "{ a = a }.b"
      --   `shouldParse` (Project (Record [("a", Var "a")]) "b")
  describe "parsing char literals" $ do
    it "parses a simple char" $ do
      parse' pExpr "" "'a'" `shouldParse` CharLit 'a'
    it "parses an escaped char" $ do
      parse' pExpr "" "'\\n'" `shouldParse` CharLit '\n'
    it "rejects invalid escape sequences" $ do
      parse' pExpr "" `shouldFailOn` "'\\a'"
    it "rejects single-quoted strings" $ do
      parse' pExpr "" `shouldFailOn` "'ab'"
  describe "parsing string literals" $ do
    it "parses a simple string" $ do
      parse' pExpr "" "\"hello\"" `shouldParse` StringLit "hello"
    it "parses a string with escaped double quotes" $ do
      parse' pExpr "" "\"hello quote: \\\"\""
        `shouldParse` StringLit "hello quote: \""
    it "parses a string with an escaped backslash" $ do
      parse' pExpr "" "\"hello backslash: \\\\\""
        `shouldParse` StringLit "hello backslash: \\"
    it "parses a string with an escaped newline" $ do
      parse' pExpr "" "\"hello newline: \\n\""
        `shouldParse` StringLit "hello newline: \n"
    it "parses a string with an interpolation (1)" $ do
      parse' pExpr "" "\"hello #{name}\""
        `shouldParse` StringInterp "hello " [(Var "name", "")]
    it "parses a string with an interpolation (2)" $ do
      parse' pExpr "" "\"hello #{5}\""
        `shouldParse` StringInterp "hello " [(IntLit 5, "")]
    it "parses a string with an interpolation (3)" $ do
      parse' pExpr "" "\"hello #{True} there\""
        `shouldParse` StringInterp "hello " [(BoolLit True, " there")]
    it "parses a string with a lone hash" $ do
      parse' pExpr "" "\"hello hash: #\""
        `shouldParse` StringLit "hello hash: #"
    it "parses a string with an escaped hash bracket" $ do
      parse' pExpr "" "\"hello hash bracket: #\\{\""
        `shouldParse` StringLit "hello hash bracket: #{"
    it "parses a string with several escaped backslashes" $ do
      parse' pExpr "" "\"\\\\\\\\\"" `shouldParse` StringLit "\\\\"
  describe "parsing types" $ do
    it "basic applications" $ do
      parse' pType "" "f a" `shouldParse` (TyVar "f" `tyapp` TyVar "a")
    it "applications on the left of applications" $ do
      parse' pType "" "f a b"
        `shouldParse` (TyVar "f" `tyapp` TyVar "a" `tyapp` TyVar "b")
    it "applications on the right of applications" $ do
      parse' pType "" "f (m a)"
        `shouldParse` (TyVar "f" `tyapp` (TyVar "m" `tyapp` TyVar "a"))
    it "applications on the left of arrows" $ do
      parse' pType "" "f a -> b"
        `shouldParse` (TyVar "f" `tyapp` TyVar "a" `fn` TyVar "b")
    it "applications on the right of arrows" $ do
      parse' pType "" "a -> f a"
        `shouldParse` (TyVar "a" `fn` TyVar "f" `tyapp` TyVar "a")
    -- This is not well-typed but should still parse
    it "arrows on the left of applications" $ do
      parse' pType "" "(a -> b) f"
        `shouldParse` ((TyVar "a" `fn` TyVar "b") `tyapp` TyVar "f")
    it "arrows on the right of applications" $ do
      parse' pType "" "f (a -> b)"
        `shouldParse` (TyVar "f" `tyapp` (TyVar "a" `fn` TyVar "b"))
    it "arrows on the left of arrows" $ do
      parse' pType "" "(a -> b) -> c"
        `shouldParse` ((TyVar "a" `fn` TyVar "b") `fn` TyVar "c")
    it "arrows on the right of arrows" $ do
      parse' pType "" "a -> b -> c"
        `shouldParse` (TyVar "a" `fn` TyVar "b" `fn` TyVar "c")
    it "arrows on the right of arrows (with parens)" $ do
      parse' pType "" "a -> (b -> c)"
        `shouldParse` (TyVar "a" `fn` TyVar "b" `fn` TyVar "c")
    it "parses basic function types" $ do
      parse' pType "" "a -> b" `shouldParse` (TyVar "a" `fn` TyVar "b")
    it "parses multi arg function types" $ do
      parse' pType "" "a -> b -> c"
        `shouldParse` (TyVar "a" `fn` TyVar "b" `fn` TyVar "c")
    it "parses higher order function types" $ do
      parse' pType "" "(a -> b) -> a -> b"
        `shouldParse` ((TyVar "a" `fn` TyVar "b") `fn` TyVar "a" `fn` TyVar "b")
    it "parses multi parameter type constructors" $ do
      parse' pType "" "Either a b -> Maybe a"
        `shouldParse` (       TyCon "Either"
                      `tyapp` TyVar "a"
                      `tyapp` TyVar "b"
                      `fn`    TyCon "Maybe"
                      `tyapp` TyVar "a"
                      )
    it "parses parameterised constructors inside lists" $ do
      parse' pType "" "[Maybe a]"
        `shouldParse` TyApp TyList (TyCon "Maybe" `tyapp` TyVar "a")
    it "parses function types in lists" $ do
      parse' pType "" "[a -> b]"
        `shouldParse` TyApp TyList (TyVar "a" `fn` TyVar "b")
    it "parses function types in higher kinded types" $ do
      parse' pType "" "A [a -> a]"
        `shouldParse` TyApp
                        (TyCon "A")
                        (TyApp TyList (TyVar "a" `fn` TyVar "a"))
      parse' pType "" "A [B -> C]"
        `shouldParse` TyApp
                        (TyCon "A")
                        (TyApp TyList (TyCon "B" `fn` TyCon "C"))
    it "parses nested type constructors" $ do
      parse' pType "" "A B" `shouldParse` TyApp (TyCon "A") (TyCon "B")
    it "parses record types" $ do
      parse' pType "" "{x : a, y : b}"
        `shouldParse` TyRecord [("x", TyVar "a"), ("y", TyVar "b")]
    it "parses applications of holes to types" $ do
      parse' pType "" "?a A" `shouldParse` (TyHole "a" `tyapp` TyCon "A")
    it "parses quantification" $ do
      parse' pType "" "forall a. a -> a"
        `shouldParse` TyForall "a" (TyVar "a" `fn` TyVar "a")
    it "parses multi quantification" $ do
      parse' pType "" "forall a b. a -> b"
        `shouldParse` TyForall "a" (TyForall "b" (TyVar "a" `fn` TyVar "b"))
    it "parses complex multi quantification" $ do
      parse' pType "" "forall a b. (a -> b) -> f a -> f b"
        `shouldParse` TyForall
                        "a"
                        (TyForall
                          "b"
                          (    (TyVar "a" `fn` TyVar "b")
                          `fn` (TyVar "f" `tyapp` TyVar "a")
                          `fn` (TyVar "f" `tyapp` TyVar "b")
                          )
                        )

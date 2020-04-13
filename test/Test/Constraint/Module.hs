module Test.Constraint.Module
  ( test
  )
where

import           Test.Hspec
import           Syn                     hiding ( Name
                                                , Scheme
                                                )
import qualified Canonical                     as Can
import           Canonical                      ( Name(..) )
import           Constraint.Generate.Module     ( generateModule )
import           Constraint.Expr         hiding ( Exp(..) )
import           Constraint                     ( Type(..)
                                                , Var(..)
                                                , Constraint(..)
                                                )
import qualified Constraint                    as C
                                                ( fn )
import           Constraint.Print

import           Constraint.Generate.M          ( run
                                                , TypeEnv
                                                )
import qualified Data.Map.Strict               as Map
import           Util

test :: Spec
test = describe "typing simple modules" $ do
  it "five : Int; five = 5" $ do
    infersType mempty
               (mkModule [("foo", TyInt, [([], IntLit 5)])])
               [("foo", Forall [] mempty TInt)]
  it "id : a -> a; id x = x" $ do
    infersType
      mempty
      (mkModule [("id", TyVar "a" `fn` TyVar "a", [([VarPat "x"], Var "x")])])
      [("id", Forall [R "a"] mempty (TVar (R "a") `C.fn` TVar (R "a")))]
  it "const : a -> b -> a; const x y = x" $ do
    infersType
      mempty
      (mkModule
        [ ( "const"
          , TyVar "a" `fn` (TyVar "b" `fn` TyVar "a")
          , [([VarPat "x", VarPat "y"], Var "x")]
          )
        ]
      )
      [ ( "const"
        , Forall [R "a", R "b"]
                 mempty
                 (TVar (R "a") `C.fn` (TVar (R "b") `C.fn` TVar (R "a")))
        )
      ]
  it
      "data Maybe a = Just a | Nothing; fromMaybe : Maybe a -> a -> a; fromMaybe (Just x) _ = x; fromMaybe Nothing y = y"
    $ do
        let maybe = DataDecl $ Data
              { dataName = "Maybe"
              , dataTyVars = ["a"]
              , dataCons = [ DataCon { conName = "Just", conArgs = [TyVar "a"] }
                           , DataCon { conName = "Nothing", conArgs = [] }
                           ]
              }
            fromMaybe = mkDecl
              ( "fromMaybe"
              , TyCon "Maybe" [TyVar "a"] `fn` (TyVar "a" `fn` TyVar "a")
              , [ ([ConsPat "Just" [VarPat "x"], WildPat], Var "x")
                , ([WildPat, VarPat "y"]                 , Var "y")
                ]
              )
            modul = Module { moduleName     = "Test"
                           , moduleMetadata = []
                           , moduleImports  = []
                           , moduleExports  = []
                           , moduleDecls    = [maybe, fromMaybe]
                           }
        infersType
          mempty
          modul
          [ ( "fromMaybe"
            , Forall
              [R "a"]
              mempty
              (TCon "Maybe" [TVar (R "a")] `C.fn` TVar (R "a") `C.fn` TVar
                (R "a")
              )
            )
          ]
  it
      "class Foo a where bar : a -> a; data Foo = Foo; instance F Foo; id_ : F a => a -> a; id_ x = bar x; foo : Foo; foo = id_ Foo"
    $ do
        let dFoo = DataDecl $ Data
              { dataName   = "Foo"
              , dataTyVars = []
              , dataCons   = [DataCon { conName = "Foo", conArgs = [] }]
              }
            fClass = TypeclassDecl $ Typeclass
              { typeclassName   = "F"
              , typeclassTyVars = ["a"]
              , typeclassDefs   = [("bar", TyVar "a" `fn` TyVar "a")]
              }
            inst = TypeclassInst $ Instance { instanceName  = "F"
                                            , instanceTypes = [TyCon "Foo" []]
                                            , instanceDefs  = []
                                            }
            id_ = FunDecl $ defaultFun
              { funName       = "id_"
              , funConstraint = Just (CInst "F" [TyVar "a"])
              , funType       = Just (TyVar "a" `fn` TyVar "a")
              , funDefs       = [ Def { defName = "id_"
                                      , defArgs = [VarPat "x"]
                                      , defExpr = App (Var "bar") (Var "x")
                                      }
                                ]
              }
            foo = FunDecl $ defaultFun
              { funComments   = []
              , funName       = "foo"
              , funConstraint = Nothing
              , funType       = Just (TyCon "Foo" [])
              , funDefs       = [ Def { defName = "foo"
                                      , defArgs = []
                                      , defExpr = App (Var "id_") (Con "Foo")
                                      }
                                ]
              }
            modul = Module { moduleName     = "Test"
                           , moduleMetadata = []
                           , moduleImports  = []
                           , moduleExports  = []
                           , moduleDecls    = [dFoo, inst, id_, foo, fClass]
                           }
        infersType
          mempty
          modul
          [ ("foo", Forall [] mempty (TCon "Foo" []))
          , ( "id_"
            , Forall [R "a"]
                     (Inst "F" [TVar (R "a")])
                     (TVar (R "a") `C.fn` TVar (R "a"))
            )
          ]

  describe "expected typing failures" $ do
    it "id : a -> a; id x = 5" $ do
      infersError [("id", TyVar "a" `fn` TyVar "a", [([VarPat "x"], IntLit 5)])]
    it "const : a -> b -> a; const x y = y" $ do
      infersError
        [ ( "const"
          , TyVar "a" `fn` (TyVar "b" `fn` TyVar "a")
          , [([VarPat "x", VarPat "y"], Var "y")]
          )
        ]

infersType :: TypeEnv -> Can.Module -> [(Name, Scheme)] -> Expectation
infersType env input outputs =
  let (res, _) = run (generateModule (mempty, env) input)
  in  case res of
        Left err -> failure (printError err)
        Right ((_, env'), _) ->
          mapM_ (\(n, s) -> Map.lookup n env' `shouldBe` Just s) outputs

infersError :: [(Name, Type_ Name, [([Pattern_ Name], Can.Exp)])] -> Expectation
infersError input =
  let (res, _) = run (generateModule mempty (mkModule input))
  in  case res of
        Left _ -> pure ()
        Right t ->
          expectationFailure $ "Expected type error, but succeeded: " <> show t

failure :: Show a => a -> Expectation
failure x = expectationFailure (show x)

mkModule
  :: [(Name, Type_ Name, [([Pattern_ Name], Can.Exp)])]
  -> Module_ Name Can.Exp (Type_ Name)
mkModule decls = Module { moduleName     = "Test"
                        , moduleMetadata = []
                        , moduleImports  = []
                        , moduleExports  = []
                        , moduleDecls    = map mkDecl decls
                        }

mkDecl
  :: (Name, Type_ Name, [([Pattern_ Name], Can.Exp)])
  -> Decl_ Name Can.Exp (Type_ Name)
mkDecl (name, ty, equations) = FunDecl $ defaultFun
  { funName = name
  , funType = Just ty
  , funDefs = map (uncurry (Def name)) equations
  }

-- This is useful to avoid adding lots of extra lines to these tests if we add a
-- new field to Fun.
defaultFun = Fun { funComments   = []
                 , funName       = "default"
                 , funType       = Nothing
                 , funConstraint = Nothing
                 , funDefs       = []
                 , funWhere      = []
                 }

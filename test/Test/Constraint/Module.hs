module Test.Constraint.Module
  ( test
  )
where

import           Test.Hspec
import           Syntax                  hiding ( Name )
import           Canonical                      ( Name(..) )
import           Constraint.Generate.Bind       ( BindT(..) )
import           Constraint.Generate.Module     ( generateModule )
import           Constraint.Expr         hiding ( Exp(..) )
import           Constraint                     ( Type(..)
                                                , Var(..)
                                                )
import qualified Constraint                    as C
                                                ( fn )

import           Constraint.Generate.M          ( run
                                                , Env
                                                )

test :: Spec
test = describe "typing simple modules" $ do
  it "five : Int; five = 5" $ do
    infersType mempty
               (mkModule [("foo", TyInt, [([], IntLit 5)])])
               [BindT "foo" [([], IntLitT 5 TInt)] (Forall [] mempty TInt)]
  it "id : a -> a; id x = x" $ do
    infersType
      mempty
      (mkModule [("id", TyVar "a" `fn` TyVar "a", [([VarPat "x"], Var "x")])])
      [ BindT "id"
              [([VarPat "x"], VarT "x" (TVar (R "a")))]
              (Forall [R "a"] mempty (TVar (R "a") `C.fn` TVar (R "a")))
      ]
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
      [ BindT
          "const"
          [([VarPat "x", VarPat "y"], VarT "x" (TVar (R "a")))]
          (Forall [R "a", R "b"]
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
              , (TyCon "Maybe" :@: TyVar "a") `fn` (TyVar "a" `fn` TyVar "a")
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
          [ BindT
              (Local "fromMaybe")
              [ ( [ConsPat "Just" [VarPat "x"], WildPat]
                , VarT "x" (TVar (R "a"))
                )
              , ([WildPat, VarPat "y"], VarT "y" (TVar (R "a")))
              ]
              (Forall
                [R "a"]
                mempty
                (TCon "Maybe" [TVar (R "a")] `C.fn` TVar (R "a") `C.fn` TVar
                  (R "a")
                )
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

infersType :: Env -> Module_ Name (Syn_ Name) -> [BindT] -> Expectation
infersType env input output =
  let (res, _) = run (generateModule env input) in res `shouldBe` Right output

infersError :: [(Name, Ty_ Name, [([Pattern_ Name], Syn_ Name)])] -> Expectation
infersError input =
  let (res, _) = run (generateModule mempty (mkModule input))
  in  case res of
        Left e -> pure ()
        Right t ->
          expectationFailure $ "Expected type error, but succeeded: " <> show t

mkModule
  :: [(Name, Ty_ Name, [([Pattern_ Name], Syn_ Name)])]
  -> Module_ Name (Syn_ Name)
mkModule decls = Module { moduleName     = "Test"
                        , moduleMetadata = []
                        , moduleImports  = []
                        , moduleExports  = []
                        , moduleDecls    = map mkDecl decls
                        }

mkDecl
  :: (Name, Ty_ Name, [([Pattern_ Name], Syn_ Name)]) -> Decl_ Name (Syn_ Name)
mkDecl (name, ty, equations) = FunDecl $ Fun
  { funComments   = []
  , funName       = name
  , funType       = ty
  , funConstraint = Nothing
  , funDefs       = map (uncurry Def) equations
  }

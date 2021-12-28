{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Type where

import           Control.Monad                  ( replicateM_ )
import qualified Data.Map.Strict               as Map
import           Data.Name                      ( Name
                                                , prim
                                                )
import           Test.Hspec
import           Type                           ( Error(..)
                                                , LocatedError(..)
                                                , check
                                                , defaultTypeEnv
                                                , fv
                                                , infer
                                                , newU
                                                , quantify
                                                , runTypeM
                                                , runTypeMAndSolve
                                                , runTypecheckM
                                                , subst
                                                , withCtorInfo
                                                , withGlobalCtx
                                                , withGlobalTypeCtx
                                                )
-- Type.DSL.fn clashes with Test.QQ.fn
import qualified Type.DSL                      as T
                                                ( fn )
import           Type.DSL                       ( forAll
                                                , ifn
                                                , tapp
                                                , tcon
                                                , trecord
                                                , u_
                                                , u_'
                                                )
import           Type.Module                    ( checkModule
                                                , typecheckFun
                                                )
import           Type.Primitive                 ( listConsMeta )
import           Type.Primitive                 ( bool
                                                , int
                                                , list
                                                , string
                                                )
import           Type.Print                     ( printLocatedError )
import           Type.Type                      ( CtorInfo
                                                , Ctx
                                                , CtxElem(..)
                                                , Type(..)
                                                , TypeCtx
                                                , U(..)
                                                )

import           AST
import           Canonicalise                   ( canonicaliseExp
                                                , canonicaliseModule
                                                , canonicaliseType
                                                )
import qualified Syn
import           Syn.Typed                      ( typeOf )
import qualified Syn.Typed                     as T
import           Test.QQ
import           Type.FromSyn                   ( convertType
                                                , fromSyn
                                                )

test :: Spec
test = do
  describe "check [Nothing] : forall a. [Maybe a]" $ do
    let a0 = U 0 "a"
        a1 = U 1 "a"
        maybeType arg = tcon "Maybe" [arg]
        listType arg = tcon (prim "List") [arg]
        ctx      = [V "Nothing" (forAll a0 (maybeType (u_ a0)))]
        ty       = forAll a1 (list (maybeType (u_ a1)))
        expr     = App (App (Var (prim "::")) (Var "Nothing")) (Var (prim "[]"))
        r        = check expr ty
        expected = AppT
          ty
          (AppT
            (T.fn (listType (maybeType (u_ a1))) (listType (maybeType (u_ a1))))
            (VarT
              (T.fn
                (maybeType (u_ a1))
                (T.fn (listType (maybeType (u_ a1)))
                      (listType (maybeType (u_ a1)))
                )
              )
              (prim "::")
            )
            (VarT (maybeType (u_ a1)) "Nothing")
          )
          (VarT (listType (maybeType (u_ a1))) (prim "[]"))

    it "typechecks successfully" $ do
      runTypecheckM defaultTypeEnv (withGlobalCtx (<> ctx) (runTypeMAndSolve r))
        `shouldBe` Right expected
  describe "check foo = (Nothing :: rest) -> foo rest : [Maybe a] -> [a]" $ do
    -- Note that we add the (claimed) type for foo to the context so that the
    -- recursive call can be inferred.
    -- We do this for functions normally anyway (see 'Type.Module.checkModule')
    let
      a0 = U 0 "a"
      a1 = U 1 "a"
      maybeType arg = tcon "Maybe" [arg]
      funType = forAll a0 (T.fn (list (maybeType (u_ a0))) (list (u_ a0)))
      cons    = prim "::"
      nothing = "Nothing"
      fun     = MCase
        [ ( [ ConsPat ()
                      cons
                      Nothing
                      [ConsPat () nothing Nothing [], VarPat () "rest"]
            ]
          , App (Var "foo") (Var "rest")
          )
        ]
      ctx         = [V nothing (forAll a1 (maybeType (u_ a1))), V "foo" funType]
      nothingMeta = ConMeta { conMetaTag      = 0
                            , conMetaArity    = 0
                            , conMetaTypeName = "Maybe"
                            }
      ctorInfo = [("Nothing", nothingMeta)]
      expected = MCaseT
        funType
        [ ( [ ConsPat
                (list (maybeType (u_ a0)))
                cons
                (Just listConsMeta)
                [ ConsPat (tcon "Maybe" [u_ a0]) nothing (Just nothingMeta) []
                , VarPat (list (maybeType (u_ a0))) "rest"
                ]
            ]
          , AppT (list (u_ a0))
                 (VarT (T.fn (list (maybeType (u_ a0))) (list (u_ a0))) "foo")
                 (VarT (list (maybeType (u_ a0))) "rest")
          )
        ]
      r = check fun funType
    it "typechecks successfully" $ do
      runTypecheckM
          defaultTypeEnv
          (withCtorInfo (<> ctorInfo)
                        (withGlobalCtx (<> ctx) (runTypeMAndSolve r))
          )
        `shouldBe` Right expected
  describe "implicit arguments" $ do
    let
      fType = ifn (tcon "A" []) $ T.fn (tcon "B" []) (tcon "C" [])
      tctx  = [("A", ()), ("B", ()), ("C", ())]
      ctx =
        [ V "a" (tcon "A" [])
        , V "b" (tcon "B" [])
        , V "c" (tcon "C" [])
        , V "f" fType
        ]
      expr = App (Var "f") (Var "b")
      fun  = Syn.Fun { Syn.funName     = "foo"
                     , Syn.funType     = Just $ Syn.TyCon "C"
                     , Syn.funExpr     = expr
                     , Syn.funWheres   = []
                     , Syn.funComments = []
                     }
      arg = (fun, ("foo", Just (tcon "C" []), expr))
    describe "check foo = (f :: A => B -> C) b : C" $ do
      let expected = T.Fun
            { T.funName   = "foo"
            , T.funType   = tcon "C" []
            , T.funExpr   = AppT
                              (tcon "C" [])
                              (AppT (T.fn (tcon "B" []) (tcon "C" []))
                                    (VarT fType "f")
                                    (ImplicitT (tcon "A" []) (Solved "a"))
                              )
                              (VarT (tcon "B" []) "b")
            , T.funWheres = []
            }
      it "typechecks successfully" $ do
        runTypecheckM
            defaultTypeEnv
            (withGlobalTypeCtx (const tctx)
                               (withGlobalCtx (const ctx) (typecheckFun arg))
            )
          `shouldBe` Right expected
    describe
        "implicit: check foo = (f :: A => B -> C) b fails when no a : A in scope"
      $ do
          let ctx2 = [V "b" (tcon "B" []), V "c" (tcon "C" []), V "f" fType]
          it "fails" $ do
            runTypecheckM
                defaultTypeEnv
                (withGlobalTypeCtx
                  (const tctx)
                  (withGlobalCtx (const ctx2) (typecheckFun arg))
                )
              `shouldBe` Left
                           (LocatedError
                             (Just "foo")
                             (NoProofFound (tcon "A" []))
                           )
    describe
        "implicit: check foo = (f :: A => B -> C) b fails when multiple As in scope"
      $ do
          let ctx2 =
                [ V "a1" (tcon "A" [])
                , V "a2" (tcon "A" [])
                , V "b"  (tcon "B" [])
                , V "c"  (tcon "C" [])
                , V "f"  fType
                ]
          it "fails" $ do
            runTypecheckM
                defaultTypeEnv
                (withGlobalTypeCtx
                  (const tctx)
                  (withGlobalCtx (const ctx2) (typecheckFun arg))
                )
              `shouldBe` Left
                           (LocatedError
                             (Just "foo")
                             (MultipleProofsFound (tcon "A" []) ["a1", "a2"])
                           )
  describe "Simple inference" $ do
    let
      nat = tcon "Nat" []
      wrap a = tcon "Wrap" [a]
      pair a b = tcon "Pair" [a, b]

      tctx = map (, ()) ["Nat", "Wrap", "Pair"]
      cctx =
        [ ( qq "Zero"
          , ConMeta { conMetaTag      = 0
                    , conMetaArity    = 0
                    , conMetaTypeName = qq "Nat"
                    }
          )
        , ( qq "Suc"
          , ConMeta { conMetaTag      = 1
                    , conMetaArity    = 1
                    , conMetaTypeName = qq "Nat"
                    }
          )
        , ( qq "MkPair"
          , ConMeta { conMetaTag      = 0
                    , conMetaArity    = 2
                    , conMetaTypeName = qq "Pair"
                    }
          )
        , ( qq "MkWrap"
          , ConMeta { conMetaTag      = 0
                    , conMetaArity    = 1
                    , conMetaTypeName = qq "Wrap"
                    }
          )
        ]

      ctx =
        [ V (qq "Zero") nat
        , V (qq "Suc")  (T.fn nat nat)
        , V (qq "MkWrap")
            (let a = U 0 "a" in forAll a $ T.fn (u_ a) (wrap (u_ a)))
        , V
          (qq "MkPair")
          (let a = U 1 "a"
               b = U 2 "b"
           in  forAll a $ forAll b $ T.fn (u_ a)
                                          (T.fn (u_ b) (pair (u_ a) (u_ b)))
          )
        ]

      -- The contexts don't change for most of these tests, so define a shortcut
      inf = infers tctx cctx ctx

    it "True" $ inf [syn|True|] bool
    it "simple function application" $ inf [syn|(\x -> x) True|] bool
    it "multi-arg function application"
      $ inf [syn|(\x y -> x) True (Suc Zero)|] bool
    it "compound lets"
      $ let expr = [syn|let
                            x  = True
                            id = \y -> y
                         in id x|]
        in  inf expr bool
    it "simple case expressions"
      $ let expr = [syn|case True of
                              True -> False
                              False -> True|]
        in  inf expr bool
    it "combined case and let expressions"
      $ let expr = [syn|case True of
                         True -> let id = \y -> y
                                  in id True
                         False -> True|]
        in  inf expr bool
    it "expressions with annotated lets"
      $ let expr = [syn|let
                            id : Bool -> Bool
                            id = x -> x
                        in id True|]
        in  inf expr bool
    it "expressions with badly annotated lets"
      $ let expr = [syn|let
                            id : Char
                            id = x -> x
                         in id True|]
        in  failsToInfer
              tctx
              cctx
              ctx
              expr
              (\case
                SubtypingFailure _ _ -> True
                _                    -> False
              )
    it "simultaneous let definitions"
      $ let expr = [syn|let
                            y = True
                            x = y
                         in x|]
        in  inf expr bool
    it "case expressions with variable patterns"
      $ let expr = [syn|case True of
                         x -> Zero|]
        in  inf expr nat
    it "case expressions that use bound variables"
      $ let expr = [syn|case True of
                         False -> False
                         x -> x|]
        in  inf expr bool
    it "case expressions with wildcard patterns"
      $ let expr = [syn|case True of
                        _ -> False|]
        in  inf expr bool
    it "case expressions with a mixture of patterns"
      $ let expr = [syn|case True of
                         True -> False
                         x -> True|]
        in  inf expr bool
    it "creating an instance of a parameterised type"
      $ let expr = [syn|MkPair False Zero|] in inf expr (pair bool nat)
    it "deconstructing a parameterised type with a case expression (Wrap)"
      $ let expr = [syn|case (MkWrap True) of
                         MkWrap y -> y|]
        in  inf expr bool
    it "deconstructing a parameterised type with a case expression (Pair)"
      $ let expr = [syn|case (MkPair False Zero) of
                         MkPair y z -> MkWrap y
                         w -> MkWrap False|]
        in  inf expr (wrap bool)
    it "an expression hole (1)"
      $ let _expr = [syn|let x = ?foo in True|]
        in  pendingWith "this results in a type error: cannot infer hole"
    it "an expression hole (2)"
      $ let _expr = [syn|let
                            not = True -> False
                                  False -> True
                         in not ?foo|]
        in  pendingWith "this results in a type error: cannot check hole"
    it "a tuple"
      -- (True, False, Zero)
      $ let expr = [syn|(True, False, Zero)|]
        in  inf expr (tcon (prim "Tuple3") [bool, bool, nat])
    it "a list" $ let expr = [syn|[True, False]|] in inf expr (list bool)
    it "an integer literal" $ let expr = [syn|6|] in inf expr int
    it "a string literal" $ let expr = [syn|"Hello"|] in inf expr string
    it "a record"
      $ let expr = [syn|{ five = 5, msg = "Hello" }|]
        in  inf expr (trecord [("five", int), ("msg", string)])
    it "a record projection"
      $ let expr = [syn|let r = { five = 5, msg = "Hello" }
                         in r.five|]
        in  inf expr int
    -- fcalls have hardcoded types. putStrLn : String -> IO Unit
    -- Currently we are omitting the IO part as we figure out fcalls.
    -- This may change.
    it "a foreign call"
      $ checks tctx mempty ctx [syn|$fcall putStrLn "Hello"|] [typ|()|]
    it "simple record extraction"
        -- D : { field : Bool } -> D a
        -- f : a -> D a -> a
        -- f = x (D d) -> x
      $ let
          a0 = U 0 "a"
          ctx' =
            [ V
                (qq "D")
                (forAll
                  a0
                  (T.fn (trecord [("field", bool)]) (tcon (qq "D") [u_ a0]))
                )
            ]
          tctx' = [(qq "D", ())]
          cctx' =
            [ ( qq "D"
              , ConMeta { conMetaTag      = 0
                        , conMetaArity    = 1
                        , conMetaTypeName = qq "D"
                        }
              )
            ]
          e = [syn|x (D d) -> x|]
          t = [typ|forall a. a -> D a -> a|]
        in
          checks tctx' cctx' ctx' e t
    it "polymorphic record extraction"
      -- D : { field : a } -> D a
      -- f : a -> D a -> a
      -- f = x (D d) -> x
      $ let ctx' =
              [ V
                  (qq "D")
                  (forAll
                    (U 0 "a")
                    (T.fn (trecord [("field", u_ (U 0 "a"))])
                          (tcon (qq "D") [u_ (U 0 "a")])
                    )
                  )
              ]
            tctx' = [(qq "D", ())]
            cctx' =
              [ ( qq "D"
                , ConMeta { conMetaTag      = 0
                          , conMetaArity    = 1
                          , conMetaTypeName = qq "D"
                          }
                )
              ]
            e = [syn|x (D d) -> x|]
            t = [typ|forall a. a -> D a -> a|]
        in  checks tctx' cctx' ctx' e t
    it "polymorphic function-typed record extraction"
        -- type D a = D (a -> a)
        -- [D : (a -> a) -> D a]
        -- f : D a -> (a -> a)
        -- f = (D f) -> f
      $ let
          a0 = U 0 "a"
          ctx' =
            [ V
                (qq "D")
                (forAll a0 (T.fn (T.fn (u_ a0) (u_ a0)) (tcon (qq "D") [u_ a0]))
                )
            ]
          tctx' = [(qq "D", ())]
          cctx' =
            [ ( qq "D"
              , ConMeta { conMetaTag      = 0
                        , conMetaArity    = 1
                        , conMetaTypeName = qq "D"
                        }
              )
            ]
          t = [typ|forall a. D a -> (a -> a)|]
          e = [syn|(D f) -> f|]
        in
          checks tctx' cctx' ctx' e t
    it "higher kinded application" $ do
      -- type F t a = MkF (t a -> t a)
      -- f : T b c -> T b c
      --
      -- e : forall b c. F (T b) c
      -- e = MkF f
      let a = U 0 "a"
          b = U 1 "b"
          c = U 2 "c"
          t = U 3 "t"
          ctx' =
            [ V
              (qq "MkF")
              (forAll
                t
                (forAll
                  a
                  (T.fn (T.fn (tapp (u_' t) [u_ a]) (tapp (u_' t) [u_ a]))
                        (tcon (qq "F") [u_ t])
                  )
                )
              )
            , V
              (qq "f")
              (forAll
                b
                (forAll
                  c
                  (T.fn (tcon (qq "T") [u_ b, u_ c])
                        (tcon (qq "T") [u_ b, u_ c])
                  )
                )
              )
            ]
          tctx' = [(qq "T", ()), (qq "F", ())]
          cctx' =
            [ ( qq "MkF"
              , ConMeta { conMetaTag      = 0
                        , conMetaArity    = 1
                        , conMetaTypeName = qq "f"
                        }
              )
            ]
          e   = [syn|MkF f|]
          ty_ = [typ|forall b c. F (T b) c|]
      checks tctx' cctx' ctx' e ty_
    it "higher kinded function application (with ->)" $ do
      -- type F t a = MkF ((t -> a) -> (t -> a))
      -- f : (c -> T -> b) -> (c -> T -> b)
      --
      -- e = MkF f
      let a = U 0 "a"
          b = U 1 "b"
          c = U 2 "c"
          t = U 3 "t"
          ctx' =
            [ V
              (qq "MkF")
              (forAll
                t
                (forAll
                  a
                  (T.fn (T.fn (T.fn (u_ t) (u_ a)) (T.fn (u_ t) (u_ a)))
                        (tcon (qq "F") [u_ t, u_ a])
                  )
                )
              )
            , V
              (qq "f")
              (forAll
                b
                (forAll
                  c
                  (T.fn (T.fn (u_ c) (T.fn (tcon (qq "T") []) (u_ b)))
                        (T.fn (u_ c) (T.fn (tcon (qq "T") []) (u_ b)))
                  )
                )
              )
            ]
          tctx' = [(qq "F", ()), (qq "T", ())]
          cctx' =
            [ ( qq "MkF"
              , ConMeta { conMetaTag      = 0
                        , conMetaArity    = 1
                        , conMetaTypeName = qq "f"
                        }
              )
            ]
          e   = [syn|MkF f|]
          ty_ = [typ|forall a b. F a (T -> b)|]
      checks tctx' cctx' ctx' e ty_

infers
  :: [(Name, ())] -> [(Name, ConMeta)] -> Ctx -> Syn.Syn -> Type -> Expectation
infers tctx cctx ctx expr t = do
  case runInfer (Map.fromList tctx) (Map.fromList cctx) ctx expr of
    Left  err      -> expectationFailure $ show (printLocatedError err)
    Right resultTy -> resultTy `shouldBe` t

failsToInfer
  :: [(Name, ())]
  -> [(Name, ConMeta)]
  -> Ctx
  -> Syn.Syn
  -> (Error -> Bool)
  -> Expectation
failsToInfer tctx cctx ctx expr matchesError =
  case runInfer (Map.fromList tctx) (Map.fromList cctx) ctx expr of
    Right resultTy ->
      expectationFailure $ "Expected type error but inferred " <> show resultTy
    Left (LocatedError _ err) -> matchesError err `shouldBe` True

runInfer :: TypeCtx -> CtorInfo -> Ctx -> Syn.Syn -> Either LocatedError Type
runInfer tctx cctx ctx expr = do
  let moduleName    = "qq.QQ"
      canonicalExpr = canonicaliseExp (moduleName, mempty) mempty expr
  let r = do
        e  <- fromSyn canonicalExpr
        e' <- infer e
        ty <- subst $ typeOf e'
        quantify (fv ty) ty
  runTypecheckM defaultTypeEnv
    $   withGlobalTypeCtx (<> tctx)
    $   withGlobalCtx (<> ctx)
    $   withCtorInfo (<> cctx)
    $   fst
    <$> runTypeM r

-- Like infers but takes a quasiquoted type expression.
-- Currently unused because most of the time the type is Bool, which is easy to
-- write in AST form.
infers' :: TypeCtx -> Ctx -> Syn.Syn -> Syn.Type -> Expectation
infers' tctx ctx expr ty = do
  let moduleName    = "qq.QQ"
      canonicalExpr = canonicaliseExp (moduleName, mempty) mempty expr
      canonicalType = canonicaliseType (moduleName, mempty) ty
  let r = do
        e  <- fromSyn canonicalExpr
        t  <- convertType mempty canonicalType
        e' <- infer e
        t' <- subst $ typeOf e'
        pure (t, t')
  let result =
        runTypecheckM defaultTypeEnv
          $   withGlobalTypeCtx (<> tctx)
          $   withGlobalCtx (<> ctx)
          $   fst
          <$> runTypeM r
  case result of
    Left err -> expectationFailure $ show (printLocatedError err)
    Right (expectedType, actualType) -> actualType `shouldBe` expectedType

checks
  :: [(Name, ())]
  -> [(Name, ConMeta)]
  -> Ctx
  -> Syn.Syn
  -> Syn.Type
  -> Expectation
checks tctx cctx ctx expr ty = do
  let modul = canonicaliseModule Syn.Module
        { Syn.moduleName     = "qq.QQ"
        , Syn.moduleImports  = mempty
        , Syn.moduleExports  = mempty
        , Syn.moduleDecls    = [ Syn.FunDecl Syn.Fun { Syn.funName     = "f"
                                                     , Syn.funType     = Just ty
                                                     , Syn.funExpr     = expr
                                                     , Syn.funComments = mempty
                                                     , Syn.funWheres   = []
                                                     }
                               ]
        , Syn.moduleMetadata = mempty
        }
      -- TODO: fix this dummy thing
      r =
        runTypeM (replicateM_ 10 (newU "dummy"))
          >> checkModule (Map.fromList tctx, ctx, Map.fromList cctx) modul
          >> pure ()
  case runTypecheckM defaultTypeEnv r of
    Left  err -> expectationFailure $ show (printLocatedError err)
    Right ()  -> pure ()

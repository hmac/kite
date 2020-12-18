{-# LANGUAGE QuasiQuotes #-}
module Test.Type where

import           Test.Hspec
import           Type
import           Type.Module                    ( checkModule )
import           Type.Print                     ( printLocatedError )
import           Control.Monad                  ( replicateM_ )
import           Data.Name
import           Control.Monad.Trans.State.Strict
                                                ( put )
import           Control.Monad.Trans.Class      ( lift )
import           Util

import           Test.QQ
import           Canonicalise                   ( canonicaliseExp
                                                , canonicaliseType
                                                , canonicaliseModule
                                                )
import           Type.FromSyn                   ( fromSyn
                                                , convertType
                                                )
import qualified Syn
import           AST

test :: Spec
test = do
  describe "check [Nothing] : forall a. [Maybe a]" $ do
    let a0 = U 0 "a"
        a1 = U 1 "a"
        maybeType arg = TCon "Maybe" [arg]
        ctx  = [V (Free "Nothing") (Forall a0 (maybeType (UType a0)))]
        ty   = Forall a1 (list (maybeType (UType a1)))
        expr = App
          (App (Var (Free "Kite.Primitive.::")) (Var (Free "Nothing")))
          (Var (Free "Kite.Primitive.[]"))
        r = putCtx ctx >> check expr ty
    it "typechecks successfully" $ do
      runTypeM defaultTypeEnv r `shouldBe` Right ()
  describe "check foo = (Nothing :: rest) -> foo rest : [Maybe a] -> [a]" $ do
    -- Note that we add the (claimed) type for foo to the context so that the
    -- recursive call can be inferred.
    -- We do this for functions normally anyway (see Type.Module.checkModule)
    let maybeType arg = TCon "Maybe" [arg]
        funType = Forall
          (U 0 "a")
          (Fn (list (maybeType (UType (U 0 "a")))) (list (UType (U 0 "a"))))
        cons    = Free "Kite.Primitive.::"
        nil     = Free "Kite.Primitive.[]"
        nothing = Free "Nothing"
        fun     = MCase
          [ ( [ConsPat cons [ConsPat nothing [], VarPat (Free "rest")]]
            , App (Var (Free "foo")) (Var (Free "rest"))
            )
          ]
        ctx =
          [ V nothing (Forall (U 1 "a") (maybeType (UType (U 1 "a"))))
          , V (Free "foo") funType
          ]
    it "typechecks successfully" $ do
      let r = putCtx ctx >> check fun funType
      runTypeM (defaultTypeEnv { envCtx = primCtx <> ctx }) r
        `shouldBe` Right ()
  describe "Simple inference" $ do
    let nat    = TCon "Nat" []
        int    = TCon "Kite.Primitive.Int" []
        string = TCon "Kite.Primitive.String" []
        wrap a = TCon "Wrap" [a]
        pair a b = TCon "Pair" [a, b]

        ctx =
          [ V (Free "QQ.Zero") nat
          , V (Free "QQ.Suc")  (Fn nat nat)
          , V (Free "QQ.MkWrap")
              (let a = U 0 "a" in Forall a $ Fn (UType a) (wrap (UType a)))
          , V
            (Free "QQ.MkPair")
            (let a = U 1 "a"
                 b = U 2 "b"
             in  Forall a $ Forall b $ Fn
                   (UType a)
                   (Fn (UType b) (pair (UType a) (UType b)))
            )
          ]

    it "True" $ infers ctx [syn|True|] bool
    it "simple function application" $ infers ctx [syn|(\x -> x) True|] bool
    it "multi-arg function application"
      $ infers ctx [syn|(\x y -> x) True (Suc Zero)|] bool
    it "compound lets"
      $ let expr = [syn|let
                            x  = True
                            id = \y -> y
                         in id x|]
        in  infers ctx expr bool
    it "simple case expressions"
      $ let expr = [syn|case True of
                              True -> False
                              False -> True|]
        in  infers ctx expr bool
    it "combined case and let expressions"
      $ let expr = [syn|case True of
                         True -> let id = \y -> y
                                  in id True
                         False -> True|]
        in  infers ctx expr bool
    it "expressions with annotated lets"
      $ let expr = [syn|let
                            id : Bool -> Bool
                            id = x -> x
                        in id True|]
        in  infers ctx expr bool
    it "expressions with badly annotated lets"
      $ let expr = [syn|let
                            id : Char
                            id = x -> x
                         in id True|]
        in  failsToInfer
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
        in  infers ctx expr bool
    it "case expressions with variable patterns"
      $ let expr = [syn|case True of
                         x -> Zero|]
        in  infers ctx expr nat
    it "case expressions that use bound variables"
      $ let expr = [syn|case True of
                         False -> False
                         x -> x|]
        in  infers ctx expr bool
    it "case expressions with wildcard patterns"
      $ let expr = [syn|case True of
                        _ -> False|]
        in  infers ctx expr bool
    it "case expressions with a mixture of patterns"
      $ let expr = [syn|case True of
                         True -> False
                         x -> True|]
        in  infers ctx expr bool
    it "creating an instance of a parameterised type"
      $ let expr = [syn|MkPair False Zero|] in infers ctx expr (pair bool nat)
    it "deconstructing a parameterised type with a case expression (Wrap)"
      $ let expr = [syn|case (MkWrap True) of
                         MkWrap y -> y|]
        in  infers ctx expr bool
    it "deconstructing a parameterised type with a case expression (Pair)"
      $ let expr = [syn|case (MkPair False Zero) of
                         MkPair y z -> MkWrap y
                         w -> MkWrap False|]
        in  infers ctx expr (wrap bool)
    it "an expression hole (1)"
      $ let expr = [syn|let x = ?foo in True|]
        in  pendingWith "this results in a type error: cannot infer hole"
    it "an expression hole (2)"
      $ let expr = [syn|let
                            not = True -> False
                                  False -> True
                         in not ?foo|]
        in  pendingWith "this results in a type error: cannot check hole"
    it "a tuple"
      -- (True, False, Zero)
      $ let expr = [syn|(True, False, Zero)|]
        in  infers ctx expr (TCon "Kite.Primitive.Tuple3" [bool, bool, nat])
    it "a list"
      $ let expr = [syn|[True, False]|] in infers ctx expr (list bool)
    it "an integer literal" $ let expr = [syn|6|] in infers ctx expr int
    it "a string literal" $ let expr = [syn|"Hello"|] in infers ctx expr string
    it "a record"
      $ let expr = [syn|{ five = 5, msg = "Hello" }|]
        in  infers ctx expr (TRecord [("five", int), ("msg", string)])
    it "a record projection"
      $ let expr = [syn|let r = { five = 5, msg = "Hello" }
                         in r.five|]
        in  infers ctx expr int
    -- fcalls have hardcoded types. putStrLn : String -> IO Unit
    it "a foreign call" $ checks ctx [syn|$fcall putStrLn "Hello"|] [ty|IO ()|]
    it "simple record extraction"
        -- This passes
        -- D : { field : Bool } -> D a
        -- f : a -> D a -> a
        -- f = x (D d) -> x
      $ let
          a0 = U 0 "a"
          ctx' =
            [ V
                (Free "QQ.D")
                (Forall
                  a0
                  (Fn (TRecord [("field", bool)]) (TCon "QQ.D" [UType a0]))
                )
            ]
          e = [syn|x (D d) -> x|]
          t = [ty|forall a. a -> D a -> a|]
        in
          checks ctx' e t
    it "polymorphic record extraction"
      -- This fails
      -- D : { field : a } -> D a
      -- f : a -> D a -> a
      -- f = x (D d) -> x
      $ let ctx' =
              [ V
                  (Free "QQ.D")
                  (Forall
                    (U 0 "a")
                    (Fn (TRecord [("field", UType (U 0 "a"))])
                        (TCon "QQ.D" [UType (U 0 "a")])
                    )
                  )
              ]
            e = [syn|x (D d) -> x|]
            t = [ty|forall a. a -> D a -> a|]
        in  checks ctx' e t
    it "polymorphic function-typed record extraction"
        -- This ???
        -- type D a = D (a -> a)
        -- [D : (a -> a) -> D a]
        -- f : D a -> (a -> a)
        -- f = (D f) -> f
      $ let
          a0 = U 0 "a"
          a1 = U 1 "a"
          ctx' =
            [ V
                (Free "QQ.D")
                (Forall
                  a0
                  (Fn (Fn (UType a0) (UType a0)) (TCon "QQ.D" [UType a0]))
                )
            ]
          t = [ty|forall a. D a -> (a -> a)|]
          e = [syn|(D f) -> f|]
        in
          checks ctx' e t
    it "higher kinded application" $ do
      let a = U 0 "a"
          b = U 1 "b"
          c = U 2 "c"
          t = U 3 "t"
          ctx =
            [ V
              (Free "QQ.MkF")
              (Forall
                t
                (Forall
                  a
                  (Fn
                    (Fn (TApp (UType t) [UType a]) (TApp (UType t) [UType a]))
                    (TCon "F" [UType t])
                  )
                )
              )
            , V
              (Free "QQ.f")
              (Forall
                b
                (Forall
                  c
                  (Fn (TCon "T" [UType b, UType c])
                      (TCon "T" [UType b, UType c])
                  )
                )
              )
            ]
          e   = [syn|MkF f|]
          ty_ = [ty|forall b. F (T b)|]
      checks ctx e ty_
    it "higher kinded application (with ->)" $ do
      let
        a = U 0 "a"
        b = U 1 "b"
        c = U 2 "c"
        t = U 3 "t"
        ctx =
          [ V
            (Free "QQ.MkF")
            (Forall
              t
              (Forall
                a
                (Fn (Fn (Fn (UType t) (UType a)) (Fn (UType t) (UType a)))
                    (TCon "F" [UType t])
                )
              )
            )
          , V
            (Free "QQ.f")
            (Forall
              b
              (Forall
                c
                (Fn (Fn (UType c) (Fn (TCon "T" []) (UType b)))
                    (Fn (UType c) (Fn (TCon "T" []) (UType b)))
                )
              )
            )
          ]
        e = [syn|MkF f|]
      case runInfer ctx e of
        Left  err -> expectationFailure $ show (printLocatedError err)
        Right ty  -> expectationFailure $ show ty

infers :: Ctx -> Syn.Syn -> Type -> Expectation
infers ctx expr t = do
  case runInfer ctx expr of
    Left  err      -> expectationFailure $ show (printLocatedError err)
    Right resultTy -> resultTy `shouldBe` t

failsToInfer :: Ctx -> Syn.Syn -> (Error -> Bool) -> Expectation
failsToInfer ctx expr matchesError = case runInfer ctx expr of
  Right resultTy ->
    expectationFailure $ "Expected type error but inferred " <> show resultTy
  Left (LocatedError _ err) -> matchesError err `shouldBe` True

runInfer :: Ctx -> Syn.Syn -> Either LocatedError Type
runInfer ctx expr = do
  let moduleName    = "QQ"
      canonicalExpr = canonicaliseExp (moduleName, mempty) mempty expr
  let r = do
        e <- fromSyn canonicalExpr
        putCtx ctx
        ty <- infer e
        subst ty
  let env = defaultTypeEnv { envCtx = primCtx <> ctx, envDebug = False }
  runTypeM env r

-- Like infers but takes a quasiquoted type expression.
-- Currently unused because most of the time the type is Bool, which is easy to
-- write in AST form.
infers' :: Ctx -> Syn.Syn -> Syn.Type -> Expectation
infers' ctx expr ty = do
  let moduleName    = "QQ"
      canonicalExpr = canonicaliseExp (moduleName, mempty) mempty expr
      canonicalType = canonicaliseType (moduleName, mempty) ty
  let r = do
        e <- fromSyn canonicalExpr
        t <- convertType mempty canonicalType
        putCtx ctx
        t' <- infer e >>= subst
        pure (t, t')
  let env = defaultTypeEnv { envCtx = primCtx <> ctx }
  case runTypeM env r of
    Left err -> expectationFailure $ show (printLocatedError err)
    Right (expectedType, actualType) -> actualType `shouldBe` expectedType

checks :: Ctx -> Syn.Syn -> Syn.Type -> Expectation
checks ctx expr ty = do
  let modul = canonicaliseModule Syn.Module
        { Syn.moduleName     = "QQ"
        , Syn.moduleImports  = mempty
        , Syn.moduleExports  = mempty
        , Syn.moduleDecls    = [ Syn.FunDecl Syn.Fun { Syn.funName     = "f"
                                                     , Syn.funType     = Just ty
                                                     , Syn.funExpr     = expr
                                                     , Syn.funComments = mempty
                                                     }
                               ]
        , Syn.moduleMetadata = mempty
        }
      r   = replicateM_ 10 (newU "dummy") >> checkModule ctx modul >> pure ()
      env = defaultTypeEnv { envCtx = primCtx <> ctx }
  case runTypeM env r of
    Left  err -> expectationFailure $ show (printLocatedError err)
    Right ()  -> pure ()

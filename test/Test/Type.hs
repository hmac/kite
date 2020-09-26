{-# LANGUAGE QuasiQuotes #-}
module Test.Type where

import           Test.Hspec
import           Type
import           Type.Module                    ( checkModule )
import           Type.Print                     ( printLocatedError )
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
    let
      maybeType arg = TCon "Maybe" [arg]
      expr = App (App (Var (Free "Lam.Primitive.::")) (Var (Free "Nothing")))
                 (Var (Free "Lam.Primitive.[]"))
      ctx =
        [V (Free "Nothing") (Forall (U 0 "a") (maybeType (UType (U 0 "a"))))]
      ty = Forall (U 1 "a") (list (maybeType (UType (U 1 "a"))))
    it "typechecks successfully" $ do
      let r = do
            lift $ lift $ put 2
            _ <- check ctx expr ty
            pure ()
      runTypeM (defaultTypeEnv { envCtx = primCtx <> ctx }) r
        `shouldBe` Right ()
  describe "check foo = (Nothing :: rest) -> foo rest : [Maybe a] -> [a]" $ do
    -- Note that we add the (claimed) type for foo to the context so that the
    -- recursive call can be inferred.
    -- We do this for functions normally anyway (see Type.Module.checkModule)
    let maybeType arg = TCon "Maybe" [arg]
        funType = Forall
          (U 0 "a")
          (Fn (list (maybeType (UType (U 0 "a")))) (list (UType (U 0 "a"))))
        cons    = Free "Lam.Primitive.::"
        nil     = Free "Lam.Primitive.[]"
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
      let r = do
            lift $ lift $ put 2
            _ <- check ctx fun funType
            pure ()
      runTypeM (defaultTypeEnv { envCtx = primCtx <> ctx }) r
        `shouldBe` Right ()
  describe "Simple inference" $ do
    let nat    = TCon "Nat" []
        int    = TCon "Lam.Primitive.Int" []
        string = TCon "Lam.Primitive.String" []
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
      $ let expr = [syn|let x = True
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
      $ pendingWith "Cannot parse annotated lets yet"
      -- $ let expr = [syn|let id : a -> a
      --                       id = \x -> x
      --                   in id True|]
      --   in  infers ctx expr bool
    it "simultaneous let definitions"
      $ let expr = [syn|let y = True
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
      $ let expr = [syn|let x = ?foo
                         in True|]
        in  pendingWith "this results in a type error: cannot infer hole"
    it "an expression hole (2)"
      $ let expr = [syn|let not = True -> False
                                  False -> True
                         in not ?foo|]
        in  pendingWith "this results in a type error: cannot check hole"
    it "a tuple"
      -- (True, False, Zero)
      $ let expr = [syn|(True, False, Zero)|]
        in  infers ctx expr (TCon "Lam.Primitive.Tuple3" [bool, bool, nat])
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

infers :: Ctx -> Syn.Syn -> Type -> Expectation
infers ctx expr t = do
  let moduleName    = "QQ"
      canonicalExpr = canonicaliseExp (moduleName, mempty) mempty expr
  let r = do
        e          <- fromSyn canonicalExpr
        (ty, ctx') <- infer ctx e
        pure (subst ctx' ty)
  let env = defaultTypeEnv { envCtx = primCtx <> ctx }
  case runTypeM env r of
    Left  err      -> expectationFailure $ show (printLocatedError err)
    Right resultTy -> resultTy `shouldBe` t

-- Like infers but takes a quasiquoted type expression.
-- Currently unused because most of the time the type is Bool, which is easy to
-- write in AST form.
infers' :: Ctx -> Syn.Syn -> Syn.Type -> Expectation
infers' ctx expr ty = do
  let moduleName    = "QQ"
      canonicalExpr = canonicaliseExp (moduleName, mempty) mempty expr
      canonicalType = canonicaliseType (moduleName, mempty) ty
  let r = do
        e          <- fromSyn canonicalExpr
        t          <- convertType mempty canonicalType
        (t', ctx') <- infer ctx e
        pure (t, subst ctx' t')
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
      r   = checkModule ctx modul >> pure ()
      env = defaultTypeEnv { envCtx = primCtx <> ctx }
  case runTypeM env r of
    Left  err -> expectationFailure $ show (printLocatedError err)
    Right ()  -> pure ()

module Test.Type where

import           Test.Hspec
import           Type
import           Type.Print                     ( printLocatedError )
import           Data.Name
import           Control.Monad.Trans.State.Strict
                                                ( put )
import           Control.Monad.Trans.Class      ( lift )
import           Util

test :: Spec
test = do
  describe "check [Nothing] : forall a. [Maybe a]" $ do
    let
      maybeType arg = TCon "Maybe" [arg]
      expr = App
        (App (VarExp (Free "Lam.Primitive.::")) (VarExp (Free "Nothing")))
        (VarExp (Free "Lam.Primitive.[]"))
      ctx =
        [Var (Free "Nothing") (Forall (U 0 "a") (maybeType (UType (U 0 "a"))))]
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
            , App (VarExp (Free "foo")) (VarExp (Free "rest"))
            )
          ]
        ctx =
          [ Var nothing (Forall (U 1 "a") (maybeType (UType (U 1 "a"))))
          , Var (Free "foo") funType
          ]
    it "typechecks successfully" $ do
      let r = do
            lift $ lift $ put 2
            _ <- check ctx fun funType
            pure ()
      runTypeM (defaultTypeEnv { envCtx = primCtx <> ctx }) r
        `shouldBe` Right ()
  describe "Simple inference" $ do
    let true   = Con (Free "Lam.Primitive.True")
        false  = Con (Free "Lam.Primitive.False")
        zero   = VarExp (Free "Zero")
        suc    = VarExp (Free "Suc")
        mkpair = VarExp (Free "MkPair")
        mkwrap = VarExp (Free "MkWrap")

        nat    = TCon "Nat" []
        wrap a = TCon "Wrap" [a]
        pair a b = TCon "Pair" [a, b]

        int    = TCon "Lam.Primitive.Int" []
        string = TCon "Lam.Primitive.String" []

        nil    = VarExp (Free "Lam.Primitive.[]")
        cons x xs = App (App (VarExp (Free "Lam.Primitive.::")) x) xs

        ctx =
          [ Var (Free "Zero") nat
          , Var (Free "Suc")  (Fn nat nat)
          , Var (Free "MkWrap")
                (let a = U 0 "a" in Forall a $ Fn (UType a) (wrap (UType a)))
          , Var
            (Free "MkPair")
            (let a = U 1 "a"
                 b = U 2 "b"
             in  Forall a $ Forall b $ Fn
                   (UType a)
                   (Fn (UType b) (pair (UType a) (UType b)))
            )
          ]

    it "True" $ infers ctx true bool
    it "simple function application"
      $ infers ctx (App (Lam (Free "x") (VarExp (Free "x"))) true) bool
    it "multi-arg function application"
      $ let f    = (Lam (Free "x") (Lam (Free "y") (VarExp (Free "x"))))
            expr = App (App f true) (App suc zero)
        in  infers ctx expr bool
    it "compound lets"
      -- let x = True
      --     id = \y -> y
      --  in id x
      $ let expr = Let1
              (Free "x")
              true
              (Let1 (Free "id")
                    (Lam (Free "y") (VarExp (Free "y")))
                    (App (VarExp (Free "id")) (VarExp (Free "x")))
              )
        in  pendingWith "let typechecking not implemented yet"
    it "simple case expressions"
      -- case True of
      --   True -> False
      --   False -> True
      $ let expr = Case
              true
              [ (ConsPat (Free "Lam.Primitive.True") [] , false)
              , (ConsPat (Free "Lam.Primitive.False") [], true)
              ]
        in  infers ctx expr bool
    it "combined case and let expressions"
      -- case True of
      --   True -> let id = \y -> y
      --            in id True
      --   False -> True
      $ let expr = Case
              true
              [ ( ConsPat (Free "Lam.Primitive.True") []
                , Let1 (Free "id")
                       (Lam (Free "y") (VarExp (Free "y")))
                       (App (VarExp (Free "id")) true)
                )
              , (ConsPat (Free "Lam.Primitive.False") [], true)
              ]
        in  pendingWith "let typechecking not implemented yet"
    it "expressions with annotated lets"
      -- let id : a -> a
      --     id = \x -> x
      --  in id True

      $ pendingWith "let annotations not implemented yet"
    it "simultaneous let definitions"
      -- let x = y
      --     y = True
      --  in x

      $ pendingWith "simultaneous lets not implemented yet"
    it "case expressions with variable patterns"
      -- case True of
      --   x -> Zero
      $ let expr = Case true [(VarPat (Free "x"), zero)] in infers ctx expr nat
    it "case expressions that use bound variables"
      -- case True of
      --   False -> False
      --   x -> x
      $ let expr = Case
              true
              [ (ConsPat (Free "Lam.Primitive.False") [], false)
              , (VarPat (Free "x")                      , VarExp (Free "x"))
              ]
        in  infers ctx expr bool
    it "case expressions with wildcard patterns"
      -- case True of
      --   _ -> False
      $ let expr = Case true [(WildPat, false)] in infers ctx expr bool
    it "case expressions with a mixture of patterns"
      -- case True of
      --   True -> False
      --   x -> True
      $ let expr = Case
              true
              [ (ConsPat (Free "Lam.Primitive.True") [], false)
              , (VarPat (Free "x")                     , true)
              ]
        in  infers ctx expr bool
    it "creating an instance of a parameterised type"
      -- type Pair a b = MkPair a b
      -- MkPair False Zero
      $ let expr = App (App mkpair false) zero
        in  infers ctx expr (pair bool nat)
    it "deconstructing a parameterised type with a case expression (Wrap)"
      -- type Wrap a = MkWrap a
      -- case (MkWrap True) of
      --   MkWrap y -> y
      $ let x = App mkwrap true
            expr =
              (Case
                x
                [ ( ConsPat (Free "MkWrap") [VarPat (Free "y")]
                  , VarExp (Free "y")
                  )
                ]
              )
        in  infers ctx expr bool
    it "deconstructing a parameterised type with a case expression (Pair)"
      -- type Wrap a = MkWrap a
      -- type Pair a b = MkPair a b
      -- case (MkPair False Zero) of
      --   MkPair y z -> MkWrap y
      --   w -> MkWrap False
      $ let x    = App (App mkpair false) zero
            expr = Case
              x
              [ ( ConsPat (Free "MkPair") [VarPat (Free "y"), VarPat (Free "z")]
                , App mkwrap (VarExp (Free "y"))
                )
              , (VarPat (Free "w"), App mkwrap false)
              ]
        in  infers ctx expr (wrap bool)
    it "an expression hole (1)"
      -- let x = ?foo
      --  in True
      $ let expr = Let1 (Free "x") (Hole "foo") true
        in  pendingWith "let typechecking not implemented yet"
    it "an expression hole (2)"
      -- let not b = case b of
      --               True -> False
      --               False -> True
      --  in not ?foo
      $ let expr = Let1
              (Free "not")
              (Lam
                (Free "b")
                (Case
                  (VarExp (Free "b"))
                  [ (ConsPat (Free "Lam.Primitive.True") [] , false)
                  , (ConsPat (Free "Lam.Primitive.False") [], true)
                  ]
                )
              )
              (App (VarExp (Free "not")) (Hole "foo"))
        in  pendingWith "let typechecking not implemented yet"
    it "a tuple"
      -- (True, False, Zero)
      $ let
          expr =
            App (App (App (Con (Free "Lam.Primitive.Tuple3")) true) false) zero
        in  infers ctx expr (TCon "Lam.Primitive.Tuple3" [bool, bool, nat])
    it "a list"
      -- [True, False]
      $ let expr = cons true (cons false nil) in infers ctx expr (list bool)
    it "an integer literal" $ let expr = Int 6 in infers ctx expr int
    it "a string literal" $ let expr = String "Hello" in infers ctx expr string
    it "a record"
      $ let expr = Record [("five", Int 5), ("msg", String "Hello")]
        in  infers ctx expr (TRecord [("five", int), ("msg", string)])
    it "a record projection"
      $ let record = Record [("five", Int 5), ("msg", String "Hello")]
            expr   = Project record "five"
        in  infers ctx expr int
    -- fcalls have hardcoded types:
    -- putStrLn : String -> IO Unit
    it "a foreign call"
      $ let expr = FCall "putStrLn" [String "Hello"]
            ty =
              TApp (TCon "Lam.Primitive.IO" []) [TCon "Lam.Primitive.Unit" []]
        in  infers ctx expr ty
    it "simple record extraction"
        -- This passes
        -- D : { field : Bool } -> D a
        -- f : a -> D a -> a
        -- f = x (D d) -> x
      $ let
          a0 = U 0 "a"
          a1 = U 1 "a"
          ctx' =
            [ Var
                (Free "D")
                (Forall a0
                        (Fn (TRecord [("field", bool)]) (TCon "D" [UType a0]))
                )
            ]
          e = MCase
            [ ( [VarPat (Free "x"), ConsPat (Free "D") [VarPat (Free "d")]]
              , VarExp (Free "x")
              )
            ]
          t = Forall a1 (Fn (UType a1) (Fn (TCon "D" [UType a1]) (UType a1)))
        in
          checks ctx' e t
    it "polymorphic record extraction"
      -- This fails
      -- D : { field : a } -> D a
      -- f : a -> D a -> a
      -- f = x (D d) -> x
      $ let ctx' =
              [ Var
                  (Free "D")
                  (Forall
                    (U 0 "a")
                    (Fn (TRecord [("field", UType (U 0 "a"))])
                        (TCon "D" [UType (U 0 "a")])
                    )
                  )
              ]
            e = MCase
              [ ( [VarPat (Free "x"), ConsPat (Free "D") [VarPat (Free "d")]]
                , VarExp (Free "x")
                )
              ]
            t = Forall
              (U 1 "a")
              (Fn (UType (U 1 "a"))
                  (Fn (TCon "D" [UType (U 1 "a")]) (UType (U 1 "a")))
              )
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
            [ Var
                (Free "D")
                (Forall a0 (Fn (Fn (UType a0) (UType a0)) (TCon "D" [UType a0]))
                )
            ]
          t = Forall (U 1 "a")
                     (Fn (TCon "D" [UType a1]) (Fn (UType a1) (UType a1)))
          e = MCase
            [([ConsPat (Free "D") [VarPat (Free "x")]], VarExp (Free "x"))]
        in
          checks ctx' e t

infers :: Ctx -> Exp -> Type -> Expectation
infers ctx e t = do
  let r = do
        -- hack: ensure we generate unique vars
        lift $ lift $ put 100
        (ty, ctx') <- infer ctx e
        pure (subst ctx' ty)
  let env = defaultTypeEnv { envCtx = primCtx <> ctx }
  case runTypeM env r of
    Left  err      -> expectationFailure $ show (printLocatedError err)
    Right resultTy -> resultTy `shouldBe` t

checks :: Ctx -> Exp -> Type -> Expectation
checks ctx e t = do
  let r = do
        -- hack: ensure we generate unique vars
        lift $ lift $ put 100
        _ <- check ctx e t
        pure ()
  let env = defaultTypeEnv { envCtx = primCtx <> ctx }
  case runTypeM env r of
    Left  err -> expectationFailure $ show (printLocatedError err)
    Right ()  -> pure ()

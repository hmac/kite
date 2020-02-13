module Test.Constraint.Pattern
  ( test
  )
where

import           Test.Hspec
import qualified Data.Map.Strict               as Map

import           Constraint
import           Constraint.Solve               ( solveC )
import           Constraint.Generate.M
import           Constraint.Expr
import           Constraint.Generate.Pattern
import           Constraint.Print
import           Syn                            ( Pattern_(..) )
import           Util

test :: Spec
test = do
  let
    bool = TCon "Bool" []
    nat  = TCon "Nat" []
    pair a b = TCon "Pair" [a, b]
    wrap a = TCon "Wrap" [a]

    true   = "True"
    false  = "False"
    zero   = "Zero"
    suc    = "Suc"
    mkpair = "MkPair"
    mkwrap = "MkWrap"

    env    = Map.fromList
      [ ("True" , Forall [] CNil (TCon "Bool" []))
      , ("False", Forall [] CNil (TCon "Bool" []))
      , ("Zero" , Forall [] CNil (TCon "Nat" []))
      , ("Suc"  , Forall [] CNil (TCon "Nat" [] `fn` TCon "Nat" []))
      , ( "MkWrap"
        , Forall [R "a"] CNil (TVar (R "a") `fn` TCon "Wrap" [TVar (R "a")])
        )
      , ( "MkPair"
        , Forall
          [R "a", R "b"]
          CNil
          (    TVar (R "a")
          `fn` (TVar (R "b") `fn` TCon "Pair" [TVar (R "a"), TVar (R "b")])
          )
        )
      ]

    -- Check that a type is inferred for a pattern with no annotation
    infersType :: Pattern -> Type -> Expectation
    infersType pat ty =
      let gen = fresh >>= \u -> generatePattern env (TVar u) pat
      in  case runGenerate gen of
            Left  err                -> failure (printError err)
            Right (t, constraint, _) -> do
              constraint `shouldBe` mempty
              t `shouldBe` ty
    -- Check that a type is inferred for a pattern with an annotation
    infersTypeA :: Pattern -> Type -> Expectation
    infersTypeA pat ty = case runGenerate (generatePattern env ty pat) of
      Left  err                -> failure (printError err)
      Right (t, constraint, _) -> do
        constraint `shouldBe` mempty
        t `shouldBe` ty
    infersError :: Pattern -> Maybe Type -> Expectation
    infersError pat Nothing =
      let gen = fresh >>= \u -> generatePattern env (TVar u) pat
      in  case runGenerate gen of
            Left _ -> pure ()
            Right (_, constraint, _)
              | constraint == mempty -> expectationFailure
                "Expected type error but was successful"
              | otherwise -> pure ()
    infersError pat (Just t) = case runGenerate (generatePattern env t pat) of
      Left _ -> pure ()
      Right (_, constraint, _)
        | constraint == mempty -> expectationFailure
          "Expected type error but was successful"
        | otherwise -> pure ()
    generatesEnv
      :: GenerateM (Type, CConstraint, TypeEnv)
      -> (TypeEnv -> Expectation)
      -> Expectation
    generatesEnv gen f = case runGenerate gen of
      Left  err       -> failure (printError err)
      Right (_, _, e) -> f e

  describe "Constraint solving for patterns" $ do
    it "x : Bool" $ do
      let pat = VarPat "x"
      pat `infersTypeA` bool
      generatePattern mempty bool pat
        `generatesEnv` (\e -> Map.lookup "x" e `shouldBe` Just
                         (Forall [] mempty bool)
                       )
    it "x" $ do
      let gen = do
            u <- TVar <$> fresh
            generatePattern mempty u (VarPat "x")
      case runGenerate gen of
        Left  err                    -> failure (printError err)
        Right (ty, constraint, env') -> do
          ty `shouldBe` TVar (U "1")
          constraint `shouldBe` mempty
          Map.lookup "x" env' `shouldBe` Just (Forall [] mempty ty)
    it "5" $ IntPat 5 `infersType` TInt
    it "_ : Bool" $ infersTypeA WildPat bool
    it "True : Bool" $ ConsPat true [] `infersTypeA` bool
    it "True" $ ConsPat true [] `infersType` bool
    it "MkWrap x : Wrap Bool" $ do
      let pat = ConsPat mkwrap [VarPat "x"]
      pat `infersTypeA` wrap bool
      generatePattern env (wrap bool) pat
        `generatesEnv` (\e -> Map.lookup "x" e `shouldBe` Just
                         (Forall [] mempty bool)
                       )
    it "MkWrap True : Wrap Bool" $ do
      let pat = ConsPat mkwrap [ConsPat true []]
      pat `infersTypeA` wrap bool
    it "Suc n : Nat" $ do
      let pat = ConsPat suc [VarPat "n"]
      pat `infersTypeA` nat
      generatePattern env nat pat
        `generatesEnv` (\e -> Map.lookup "n" e `shouldBe` Just
                         (Forall [] mempty nat)
                       )
    it "MkPair x Zero : Pair Bool Nat" $ do
      let pat = ConsPat mkpair [VarPat "x", ConsPat zero []]
      pat `infersTypeA` pair bool nat
      generatePattern env (pair bool nat) pat
        `generatesEnv` (\e -> Map.lookup "x" e `shouldBe` Just
                         (Forall [] mempty bool)
                       )
    it "MkPair x Zero; MkPair True y" $ do
      let pat1 = ConsPat mkpair [VarPat "x", ConsPat zero []]
      let pat2 = ConsPat mkpair [ConsPat true [], VarPat "y"]
      let gen = do
            u <- TVar <$> fresh
            (pat1Type, pat1Constraints, pat1Env) <- generatePattern env u pat1
            (pat2Type, pat2Constraints, pat2Env) <- generatePattern env u pat2
            pure
              ( [pat1Type, pat2Type]
              , pat1Constraints <> pat2Constraints
              , pat1Env <> pat2Env
              )
      case runGenerateMulti gen of
        Left  err                          -> failure (printError err)
        Right ([t1, t2], constraint, env') -> do
          constraint `shouldBe` mempty
          t1 `shouldBe` pair bool nat
          t2 `shouldBe` pair bool nat
          Map.lookup "x" env' `shouldBe` Just (Forall [] mempty bool)
          Map.lookup "y" env' `shouldBe` Just (Forall [] mempty nat)
    it "()" $ do
      let pat          = TuplePat []
      let expectedType = mkTupleType []
      pat `infersType` expectedType
    it "(x, y) : (Bool, Bool)" $ do
      let pat          = TuplePat [VarPat "x", VarPat "y"]
      let expectedType = mkTupleType [bool, bool]
      pat `infersTypeA` expectedType
      generatePattern env expectedType pat
        `generatesEnv` (\e -> do
                         Map.lookup "x" e
                           `shouldBe` Just (Forall [] mempty bool)
                         Map.lookup "y" e
                           `shouldBe` Just (Forall [] mempty bool)
                       )
    it "(Zero, False)" $ do
      let pat          = TuplePat [ConsPat zero [], ConsPat false []]
      let expectedType = mkTupleType [nat, bool]
      pat `infersType` expectedType
    it "[] : List Bool" $ do
      let pat          = ListPat []
      let expectedType = list bool
      pat `infersTypeA` expectedType
    it "[True, False]" $ do
      let pat = ListPat [ConsPat true [], ConsPat false []]
      let expectedType = list bool
      pat `infersType` expectedType
    it "\"hello\"" $ do
      let pat          = StringPat "hello"
      let expectedType = TString
      pat `infersType` expectedType
    describe "expected type failures" $ do
      it "[] : Bool" $ do
        let pat = ListPat []
        pat `infersError` Just bool
      it "[True, Zero]" $ do
        let pat = ListPat [ConsPat true [], ConsPat zero []]
        pat `infersError` Nothing

runGenerate
  :: GenerateM (Type, CConstraint, TypeEnv)
  -> Either Error (Type, Constraint, TypeEnv)
runGenerate g =
  let (res, touchables) = run g
  in  do
        (t, constraints, env') <- res
        (cs, s)                <- solveC mempty touchables mempty constraints
        pure (sub s t, cs, sub s env')

runGenerateMulti
  :: GenerateM ([Type], CConstraint, TypeEnv)
  -> Either Error ([Type], Constraint, TypeEnv)
runGenerateMulti g =
  let (res, touchables) = run g
  in  do
        (ts, constraints, env') <- res
        (cs, s)                 <- solveC mempty touchables mempty constraints
        pure (map (sub s) ts, cs, sub s env')

failure :: Show a => a -> Expectation
failure x = expectationFailure (show x)

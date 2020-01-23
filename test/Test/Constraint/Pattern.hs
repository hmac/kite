module Test.Constraint.Pattern
  ( test
  )
where

import           Test.Hspec
import qualified Data.Map.Strict               as Map

import           Util
import           Constraint
import           Constraint.Solve               ( solveC
                                                , Error(..)
                                                )
import           Constraint.Generate.M
import           Constraint.Expr
import           Constraint.Generate            ( generate
                                                , mkTupleType
                                                , Env
                                                )
import           Constraint.Generate.Pattern
import           Constraint.Print

test :: Spec
test = do
  let
    bool = TCon "Bool" []
    nat  = TCon "Nat" []
    pair a b = TCon "Pair" [a, b]
    wrap a = TCon "Wrap" [a]

    true   = C "True"
    false  = C "False"
    zero   = C "Zero"
    suc    = C "Suc"
    mkpair = C "MkPair"
    mkwrap = C "MkWrap"

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
    infersType :: Pat -> Type -> Expectation
    infersType pat ty =
      let gen = fresh >>= \u -> generatePattern env (TVar u) pat
      in  case runGenerate gen of
            Left  err                -> expectationFailure (printError err)
            Right (t, constraint, _) -> do
              constraint `shouldBe` mempty
              t `shouldBe` ty
    -- Check that a type is inferred for a pattern with an annotation
    infersTypeA :: Pat -> Type -> Expectation
    infersTypeA pat ty = case runGenerate (generatePattern env ty pat) of
      Left  err                -> expectationFailure (printError err)
      Right (t, constraint, _) -> do
        constraint `shouldBe` mempty
        t `shouldBe` ty
    infersError :: Pat -> Maybe Type -> Expectation
    infersError pat Nothing =
      let gen = fresh >>= \u -> generatePattern env (TVar u) pat
      in  case runGenerate gen of
            Left _ -> pure ()
            Right _ ->
              expectationFailure "Expected type error but was successful"
    infersError pat (Just t) = case runGenerate (generatePattern env t pat) of
      Left  _ -> pure ()
      Right _ -> expectationFailure "Expected type error but was successful"
    generatesEnv
      :: GenerateM (Type, CConstraint, Env)
      -> (Env -> Expectation)
      -> Expectation
    generatesEnv gen f = case runGenerate gen of
      Left  err       -> expectationFailure (printError err)
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
        Left  err                    -> expectationFailure (printError err)
        Right (ty, constraint, env') -> do
          ty `shouldBe` TVar (U "1")
          constraint `shouldBe` mempty
          Map.lookup "x" env' `shouldBe` Just (Forall [] mempty ty)
    it "5" $ IntPat 5 `infersType` TInt
    it "_ : Bool" $ infersTypeA WildPat bool
    it "True : Bool" $ ConPat true [] `infersTypeA` bool
    it "True" $ ConPat true [] `infersType` bool
    it "MkWrap x : Wrap Bool" $ do
      let pat = ConPat mkwrap [VarPat "x"]
      pat `infersTypeA` wrap bool
      generatePattern env (wrap bool) pat
        `generatesEnv` (\e -> Map.lookup "x" e `shouldBe` Just
                         (Forall [] mempty bool)
                       )
    it "MkWrap True : Wrap Bool" $ do
      let pat = ConPat mkwrap [ConPat true []]
      pat `infersTypeA` wrap bool
    it "Suc n : Nat" $ do
      let pat = ConPat suc [VarPat "n"]
      pat `infersTypeA` nat
      generatePattern env nat pat
        `generatesEnv` (\e -> Map.lookup "n" e `shouldBe` Just
                         (Forall [] mempty nat)
                       )
    it "MkPair x Zero : Pair Bool Nat" $ do
      let pat = ConPat mkpair [VarPat "x", ConPat zero []]
      pat `infersTypeA` pair bool nat
      generatePattern env (pair bool nat) pat
        `generatesEnv` (\e -> Map.lookup "x" e `shouldBe` Just
                         (Forall [] mempty bool)
                       )
    it "MkPair x Zero; MkPair True y" $ do
      let pat1 = ConPat mkpair [VarPat "x", ConPat zero []]
      let pat2 = ConPat mkpair [ConPat true [], VarPat "y"]
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
        Left err -> expectationFailure (printError err)
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
      let pat          = TuplePat [ConPat zero [], ConPat false []]
      let expectedType = mkTupleType [nat, bool]
      pat `infersType` expectedType
    it "[] : List Bool" $ do
      let pat          = ListPat []
      let expectedType = TCon "List" [bool]
      pat `infersTypeA` expectedType
    it "[True, False]" $ do
      let pat          = ListPat [ConPat true [], ConPat false []]
      let expectedType = TCon "List" [bool]
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
        let pat = ListPat [ConPat true [], ConPat zero []]
        pat `infersError` Nothing

runGenerate
  :: GenerateM (Type, CConstraint, Env) -> Either Error (Type, Constraint, Env)
runGenerate g =
  let ((t, constraints, env'), touchables) = run g
  in  case solveC touchables constraints of
        Left  err     -> Left err
        Right (cs, s) -> Right (sub s t, cs, sub s env')

runGenerateMulti
  :: GenerateM ([Type], CConstraint, Env)
  -> Either Error ([Type], Constraint, Env)
runGenerateMulti g =
  let ((ts, constraints, env'), touchables) = run g
  in  case solveC touchables constraints of
        Left  err     -> Left err
        Right (cs, s) -> Right (map (sub s) ts, cs, sub s env')

printError :: Error -> String
printError (OccursCheckFailure t v) =
  show
    $  "Occurs check failure between "
    <> printType t
    <> " and "
    <> printType v
printError (ConstructorMismatch t v) =
  show
    $  "Constructors do not match between "
    <> printType t
    <> " and "
    <> printType v
printError (UnsolvedConstraints c) =
  show $ "Unsolved constraints: " <> printConstraint c

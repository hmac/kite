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
import           Constraint.Generate            ( generate
                                                , Con(..)
                                                , Scheme(..)
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

  describe "Constraint solving for patterns" $ do
    it "x : Bool" $ do
      case runGenerate (generatePattern mempty bool (VarPat "x")) of
        Left  err                    -> expectationFailure (printError err)
        Right (ty, constraint, env') -> do
          ty `shouldBe` bool
          constraint `shouldBe` mempty
          Map.lookup "x" env' `shouldBe` Just (Forall [] mempty bool)
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
    it "_ : Bool" $ do
      case runGenerate (generatePattern mempty bool WildPat) of
        Left  err                    -> expectationFailure (printError err)
        Right (ty, constraint, env') -> do
          ty `shouldBe` bool
          constraint `shouldBe` mempty
          env' `shouldBe` mempty
    it "True : Bool" $ do
      let pat = ConPat true []
      case runGenerate (generatePattern env bool pat) of
        Left  err                    -> expectationFailure (printError err)
        Right (ty, constraint, env') -> do
          ty `shouldBe` bool
          constraint `shouldBe` mempty
    it "MkWrap x : Wrap Bool" $ do
      let pat = ConPat mkwrap [VarPat "x"]
      case runGenerate (generatePattern env (wrap bool) pat) of
        Left  err                    -> expectationFailure (printError err)
        Right (ty, constraint, env') -> do
          ty `shouldBe` wrap bool
          constraint `shouldBe` mempty
          Map.lookup "x" env' `shouldBe` Just (Forall [] mempty bool)
    it "MkWrap True : Wrap Bool" $ do
      let pat = ConPat mkwrap [ConPat true []]
      case runGenerate (generatePattern env (wrap bool) pat) of
        Left  err                 -> expectationFailure (printError err)
        Right (ty, constraint, _) -> do
          ty `shouldBe` wrap bool
          constraint `shouldBe` mempty
    it "Suc n : Nat" $ do
      let pat = ConPat suc [VarPat "n"]
      case runGenerate (generatePattern env nat pat) of
        Left  err                    -> expectationFailure (printError err)
        Right (ty, constraint, env') -> do
          constraint `shouldBe` mempty
          ty `shouldBe` nat
          Map.lookup "n" env' `shouldBe` Just (Forall [] mempty nat)
    it "MkPair x Zero : Pair Bool Nat" $ do
      let pat = ConPat mkpair [VarPat "x", ConPat zero []]
      case runGenerate (generatePattern env (pair bool nat) pat) of
        Left  err                    -> expectationFailure (printError err)
        Right (ty, constraint, env') -> do
          constraint `shouldBe` mempty
          ty `shouldBe` pair bool nat
          Map.lookup "x" env' `shouldBe` Just (Forall [] mempty bool)
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
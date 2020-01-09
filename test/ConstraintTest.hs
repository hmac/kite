module ConstraintTest
  ( test
  )
where

import           Test.Hspec
import qualified Hedgehog                      as H
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           HaskellWorks.Hspec.Hedgehog

import qualified Data.Map.Strict               as Map

import           Data.Name                      ( RawName(..) )
import           Constraint
import           Constraint.Solve               ( solve
                                                , solveC
                                                )
import           Constraint.Generate            ( run
                                                , generate
                                                , Exp(..)
                                                , Alt(..)
                                                , Pat(..)
                                                , Con(..)
                                                , Scheme(..)
                                                , Env
                                                )

-- Tests the constraint solver

test :: Spec
test = do
  describe "CConstraint Monoid" $ it "obeys the monoid laws" $ require
    (isLawfulMonoid genCConstraint)
  describe "Constraint Monoid" $ it "obeys the monoid laws" $ require
    (isLawfulMonoid genConstraint)

  let env = Map.fromList
        [ ("True" , Forall [] CNil (TCon "Bool" []))
        , ("False", Forall [] CNil (TCon "Bool" []))
        , ("Zero" , Forall [] CNil (TCon "Nat" []))
        , ("Suc"  , Forall [] CNil (TCon "Nat" [] `fn` TCon "Nat" []))
        ]
  describe "Constraint solving" $ do
    it "solves a simple constraint" $ do
      let c1 = TVar (U "a") :~: TCon "List" [TVar (U "b")]
      let c2 = TVar (U "a") :~: TCon "List" [TCon "Bool" []]
      solve [c1, c2] `shouldBe` Right
        [ TVar (U "b") :~: TCon "Bool" []
        , TVar (U "a") :~: TCon "List" [TCon "Bool" []]
        ]
  describe "Constraint generation" $ do
    it "unknown variables generate fresh unification vars" $ do
      run (generate mempty (Var "foo")) `shouldBe` (TVar (U "0"), Simple CNil)
    it "simple lets" $ do
      let expr = Let "x" (Con (C "True")) (Var "x")
      run (generate env expr) `shouldBe` (TCon "Bool" [], Simple CNil)
    it "compound lets" $ do
      -- let x = True
      --     id y = y
      --  in id x
      let expr = Let
            "x"
            (Con (C "True"))
            (Let "id" (Abs "y" (Var "y")) (App (Var "id") (Var "x")))
      run (generate env expr)
        `shouldBe` ( TVar (U "1")
                   , Simple
                     (   TCon "->" [TVar (U "0"), TVar (U "0")]
                     :~: TCon "->" [TCon "Bool" [], TVar (U "1")]
                     )
                   )
  describe "Constraint generate and solving combined" $ do
    it "solves simple function applications" $ do
      -- (\x -> x) True
      let expr = App (Abs "x" (Var "x")) (Con (C "True"))
      infersType env expr (TCon "Bool" [])
    it "solves multi-arg function applications" $ do
      -- (\x y -> x) True (Suc Zero)
      let constfun = Abs "x" (Abs "y" (Var "x"))
      let expr = App (App constfun (Con (C "True")))
                     (App (Con (C "Suc")) (Con (C "Zero")))
      infersType env expr (TCon "Bool" [])
    it "solves compound lets" $ do
      -- let x = True
      --     id y = y
      --  in id x
      let expr = Let
            "x"
            (Con (C "True"))
            (Let "id" (Abs "y" (Var "y")) (App (Var "id") (Var "x")))
      infersType env expr (TCon "Bool" [])
    it "solves simple case expressions" $ do
      -- case True of
      --   True -> False
      --   False -> True
      let expr = Case
            (Con (C "True"))
            [ Alt (SimpleConPat (C "True") [])  (Con (C "False"))
            , Alt (SimpleConPat (C "False") []) (Con (C "True"))
            ]
      infersType env expr (TCon "Bool" [])
    it "solves combined case and let expressions" $ do
      -- case True of
      --   True -> let id = \y -> y
      --            in id True
      --   False -> True
      let idfun = Abs "y" (Var "y")
      let expr = Case
            (Con (C "True"))
            [ Alt (SimpleConPat (C "True") [])
                  (Let "id" idfun (App (Var "id") (Con (C "True"))))
            , Alt (SimpleConPat (C "False") []) (Con (C "True"))
            ]
      infersType env expr (TCon "Bool" [])
    it "solves expressions with annotation lets" $ do
      -- let id : a -> a
      --     id = \x -> x
      --  in id True
      let expr = LetA "id"
                      (Forall [R "a"] CNil (TVar (R "a") `fn` TVar (R "a")))
                      (Abs "x" (Var "x"))
                      (App (Var "id") (Con (C "True")))
      infersType env expr (TCon "Bool" [])
    it "solves case expressions with variable patterns" $ do
      -- case True of
      --   x -> False
      let expr = Case (Con (C "True")) [Alt (VarPat "x") (Con (C "False"))]
      infersType env expr (TCon "Bool" [])
      -- case True of
      --   x -> Zero
      let expr' = Case (Con (C "True")) [Alt (VarPat "x") (Con (C "Zero"))]
      infersType env expr' (TCon "Nat" [])
    it "solves case expressions with a mixture of patterns" $ do
      -- case True of
      --   True -> False
      --   x -> True
      let expr = Case
            (Con (C "True"))
            [ Alt (SimpleConPat (C "True") []) (Con (C "False"))
            , Alt (VarPat "x")                 (Con (C "True"))
            ]
      infersType env expr (TCon "Bool" [])

infersType :: Env -> Exp -> Type -> Expectation
infersType env expr expectedType =
  let (TVar t, constraints) = run (generate env expr)
  in  case solveC constraints of
        Left err ->
          expectationFailure $ "Expected Right, found Left " <> show err
        Right (cs, s) -> do
          lookup t s `shouldBe` Just expectedType
          cs `shouldBe` []

isLawfulMonoid :: (Eq a, Show a, Monoid a) => H.Gen a -> H.Property
isLawfulMonoid gen = H.property $ do
  c <- H.forAll gen
  d <- H.forAll gen
  e <- H.forAll gen

  c <> mempty H.=== c
  mempty <> c H.=== c
  c <> (d <> e) H.=== (c <> d) <> e

-- Property test generators for Constraint, CConstraint, Type

genCConstraint :: H.Gen CConstraint
genCConstraint = Gen.recursive
  Gen.choice
  [pure mempty]
  [ Gen.subtermM
    genCConstraint
    (\c -> E <$> Gen.list (Range.linear 0 5) genVar <*> genConstraint <*> pure c
    )
  , Gen.subterm2 genCConstraint genCConstraint (<>)
  ]

genConstraint :: H.Gen Constraint
genConstraint = Gen.recursive Gen.choice
                              [pure mempty, (:~:) <$> genType <*> genType]
                              [Gen.subterm2 genConstraint genConstraint (<>)]

genType :: H.Gen Type
genType = Gen.recursive
  Gen.choice
  [TVar <$> genVar]
  [Gen.subtermM genType (\t -> TCon <$> genName <*> pure [t])]

genVar :: H.Gen Var
genVar = Gen.choice [R <$> genName, U <$> genUVarName]

genName :: H.Gen RawName
genName = Name <$> Gen.list (Range.linear 1 5) Gen.alphaNum

genUVarName :: H.Gen RawName
genUVarName = Name <$> Gen.list (Range.linear 1 5) Gen.digit

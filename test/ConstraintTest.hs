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
                                                , substExp
                                                , Exp(..)
                                                , ExpT(..)
                                                , Alt(..)
                                                , AltT(..)
                                                , Pat(..)
                                                , Con(..)
                                                , Scheme(..)
                                                , Env
                                                )

-- Tests the constraint solver

test :: Spec
test = parallel $ do
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
  let bool = TCon "Bool" []
  let nat = TCon "Nat" []
  let true = C "True"
  let false = C "False"
  let zero = C "Zero"
  let suc = C "Suc"

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
      let t = TVar (U "0")
       in run (generate mempty (Var "foo")) `shouldBe` (VarT "foo" t, t, Simple CNil)
    it "simple lets" $ do
      let expr = Let "x" (Con true) (Var "x")
      let t = TCon "Bool" []
       in run (generate env expr) `shouldBe` ( LetT "x" (ConT true) t (VarT "x" t), t, Simple CNil)
    it "compound lets" $ do
      -- let x = True
      --     id y = y
      --  in id x
      let expr = Let
            "x"
            (Con true)
            (Let "id" (Abs "y" (Var "y")) (App (Var "id") (Var "x")))
      let (_, t, c) = run (generate env expr)
      t `shouldBe` TVar (U "1")
      c `shouldBe` Simple (TCon "->" [TVar (U "0"), TVar (U "0")]
                     :~: TCon "->" [bool, TVar (U "1")]
                     )
  describe "Constraint generate and solving combined" $ do
    it "solves simple function applications" $ do
      -- (\x -> x) True
      let expr = App (Abs "x" (Var "x")) (Con true)
      infersType env expr bool
    it "solves multi-arg function applications" $ do
      -- (\x y -> x) True (Suc Zero)
      let constfun = Abs "x" (Abs "y" (Var "x"))
      let expr = App (App constfun (Con true))
                     (App (Con suc) (Con zero))
      infersType env expr bool
      let annotatedExpr =
            AppT (AppT (AbsT "x" bool (AbsT "y" nat (VarT "x" bool))) (ConT true)) (AppT (ConT (C "Suc")) (ConT (C "Zero")))
      inferAndZonk env expr annotatedExpr
    it "solves compound lets" $ do
      -- let x = True
      --     id y = y
      --  in id x
      let expr = Let
            "x"
            (Con true)
            (Let "id" (Abs "y" (Var "y")) (App (Var "id") (Var "x")))
      let annotatedExpr = LetT "x" (ConT true) bool (LetT "id" (AbsT "y" bool (VarT "y" bool)) (bool `fn` bool) (AppT (VarT "id" (bool `fn` bool)) (VarT "x" bool)))
      infersType env expr (TCon "Bool" [])
      inferAndZonk env expr annotatedExpr
    it "solves simple case expressions" $ do
      -- case True of
      --   True -> False
      --   False -> True
      let expr = Case
            (Con true)
            [ Alt (ConPat true [])  (Con false)
            , Alt (ConPat false []) (Con true)
            ]
      let annotatedExpr = CaseT (ConT true) [AltT (ConPat true []) (ConT false)
                                            , AltT (ConPat false []) (ConT true)
                                            ]
                                            bool
      infersType env expr (TCon "Bool" [])
      inferAndZonk env expr annotatedExpr
    it "solves combined case and let expressions" $ do
      -- case True of
      --   True -> let id = \y -> y
      --            in id True
      --   False -> True
      let idfun = Abs "y" (Var "y")
      let expr = Case
            (Con true)
            [ Alt (ConPat true [])
                  (Let "id" idfun (App (Var "id") (Con true)))
            , Alt (ConPat false []) (Con true)
            ]
      let annotatedExpr = CaseT (ConT true) [AltT (ConPat true []) (LetT "id" (AbsT "y" bool (VarT "y" bool)) (bool `fn` bool) (AppT (VarT "id" (bool `fn` bool)) (ConT true)))
                                            , AltT (ConPat false []) (ConT true)
                                            ]
                                            bool
      infersType env expr (TCon "Bool" [])
      inferAndZonk env expr annotatedExpr
    it "solves expressions with annotated lets" $ do
      -- let id : a -> a
      --     id = \x -> x
      --  in id True
      let idType = (Forall [R "a"] CNil (TVar (R "a") `fn` TVar (R "a")))
      let expr = LetA "id"
                      idType
                      (Abs "x" (Var "x"))
                      (App (Var "id") (Con true))
      let annotatedExpr = LetAT "id" idType (AbsT "x" bool (VarT "x" bool)) (AppT (VarT "id" (bool `fn` bool)) (ConT true)) bool
      infersType env expr (TCon "Bool" [])
      -- We can't yet deal with implication constraints in the solver, so this
      -- doesn't work yet
      -- inferAndZonk env expr annotatedExpr
    it "solves case expressions with variable patterns" $ do
      -- case True of
      --   x -> Zero
      let expr = Case (Con true) [Alt (VarPat "x") (Con (C "Zero"))]
      let annotatedExpr = CaseT (ConT true) [AltT (VarPat "x") (ConT (C "Zero"))] nat
      infersType env expr (TCon "Nat" [])
      inferAndZonk env expr annotatedExpr
    it "solves case expressions with wildcard patterns" $ do
      -- case True of
      --   _ -> False
      let expr = Case (Con true) [Alt WildPat (Con false)]
      let annotatedExpr = CaseT (ConT true) [AltT WildPat (ConT false)] bool
      infersType env expr (TCon "Bool" [])
      inferAndZonk env expr annotatedExpr
    it "solves case expressions with a mixture of patterns" $ do
      -- case True of
      --   True -> False
      --   x -> True
      let expr = Case
            (Con true)
            [ Alt (ConPat true []) (Con false)
            , Alt (VarPat "x")           (Con true)
            ]
      let annotatedExpr = CaseT (ConT true) [AltT (ConPat true []) (ConT false)
                                            , AltT (VarPat "x") (ConT true)] bool
      infersType env expr (TCon "Bool" [])
      inferAndZonk env expr annotatedExpr

infersType :: Env -> Exp -> Type -> Expectation
infersType env expr expectedType =
  let (_, TVar t, constraints) = run (generate env expr)
  in  case solveC constraints of
        Left err ->
          expectationFailure $ "Expected Right, found Left " <> show err
        Right (cs, s) -> do
          lookup t s `shouldBe` Just expectedType
          cs `shouldBe` []

inferAndZonk :: Env -> Exp -> ExpT -> Expectation
inferAndZonk env expr expectedExpr =
  let (expr', _, constraints) = run (generate env expr)
  in  case solveC constraints of
        Left err ->
          expectationFailure $ "Expected Right, found Left " <> show err
        Right (cs, s) -> do
          cs `shouldBe` []
          substExp s expr' `shouldBe` expectedExpr

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

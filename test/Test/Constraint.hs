module Test.Constraint
  ( test
  )
where

import           Test.Hspec
import qualified Hedgehog                      as H
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           HaskellWorks.Hspec.Hedgehog

import qualified Data.Map.Strict               as Map

import           Data.Name                      ( RawName(..)
                                                , ModuleName(..)
                                                )
import           Canonical                      ( Name(..) )
import           Constraint
import           Constraint.Expr
import           Constraint.Solve               ( solveC
                                                , Error(..)
                                                )
import           Constraint.Generate.M          ( run
                                                , Env
                                                )
import           Constraint.Generate            ( generate )
import           Constraint.Print
import           Syntax                         ( Pattern
                                                , Pattern_(..)
                                                )

-- Tests the constraint solver

test :: Spec
test = do
  describe "CConstraint Monoid" $ it "obeys the monoid laws" $ require
    (isLawfulMonoid genCConstraint)
  describe "Constraint Monoid" $ it "obeys the monoid laws" $ require
    (isLawfulMonoid genConstraint)

  let
    bool = TCon "Bool" []
    nat  = TCon "Nat" []
    pair a b = TCon "Pair" [a, b]
    wrap a = TCon "Wrap" [a]

    true   = "True"
    false  = "False"
    zero   = "Zero"
    suc    = "Suc"
    cons   = "::"
    mkpair = "MkPair"
    mkwrap = "MkWrap"

    env    = Map.fromList
      [ ("True" , Forall [] CNil (TCon "Bool" []))
      , ("False", Forall [] CNil (TCon "Bool" []))
      , ("Zero" , Forall [] CNil (TCon "Nat" []))
      , ("Suc"  , Forall [] CNil (TCon "Nat" [] `fn` TCon "Nat" []))
      , ( "::"
        , Forall
          [R "a"]
          CNil
          (TVar (R "a") `fn` TCon "List" [TVar (R "a")] `fn` TCon
            "List"
            [TVar (R "a")]
          )
        )
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

  describe "Constraint generation and solving combined" $ do
    it "True" $ do
      let expr = Con true
      infersType env expr bool
    it "simple function applications" $ do
      -- (\x -> x) True
      let expr = App (Abs ["x"] (Var "x")) (Con true)
      infersType env expr bool
    it "multi-arg function applications" $ do
      -- (\x y -> x) True (Suc Zero)
      let constfun = Abs ["x", "y"] (Var "x")
      let expr = App (App constfun (Con true)) (App (Con suc) (Con zero))
      infersType env expr bool
      let annotatedExpr = AppT
            (AppT (AbsT [("x", bool), ("y", nat)] (VarT "x" bool)) (ConT true))
            (AppT (ConT suc) (ConT zero))
      inferAndZonk env expr annotatedExpr
    it "compound lets" $ do
      -- let x = True
      --     id y = y
      --  in id x
      let expr = Let [("x", Con true), ("id", Abs ["y"] (Var "y"))]
                     (App (Var "id") (Var "x"))
      let annotatedExpr = LetT
            [("x", ConT true), ("id", AbsT [("y", bool)] (VarT "y" bool))]
            (AppT (VarT "id" (bool `fn` bool)) (VarT "x" bool))
            bool
      infersType env expr (TCon "Bool" [])
      inferAndZonk env expr annotatedExpr
    it "simple case expressions" $ do
      -- case True of
      --   True -> False
      --   False -> True
      let expr = Case
            (Con true)
            [ Alt (ConsPat true [])  (Con false)
            , Alt (ConsPat false []) (Con true)
            ]
      let annotatedExpr = CaseT
            (ConT true)
            [ AltT (ConsPat true [])  (ConT false)
            , AltT (ConsPat false []) (ConT true)
            ]
            bool
      infersType env expr (TCon "Bool" [])
      inferAndZonk env expr annotatedExpr
    it "combined case and let expressions" $ do
      -- case True of
      --   True -> let id = \y -> y
      --            in id True
      --   False -> True
      let idfun = Abs ["y"] (Var "y")
      let expr = Case
            (Con true)
            [ Alt (ConsPat true [])
                  (Let [("id", idfun)] (App (Var "id") (Con true)))
            , Alt (ConsPat false []) (Con true)
            ]
      let annotatedExpr = CaseT
            (ConT true)
            [ AltT
              (ConsPat true [])
              (LetT [("id", AbsT [("y", bool)] (VarT "y" bool))]
                    (AppT (VarT "id" (bool `fn` bool)) (ConT true))
                    bool
              )
            , AltT (ConsPat false []) (ConT true)
            ]
            bool
      infersType env expr (TCon "Bool" [])
      inferAndZonk env expr annotatedExpr
    it "expressions with annotated lets" $ do
      -- let id : a -> a
      --     id = \x -> x
      --  in id True
      let idType = Forall [R "a"] CNil (TVar (R "a") `fn` TVar (R "a"))
      let expr =
            LetA "id" idType (Abs ["x"] (Var "x")) (App (Var "id") (Con true))
      let annotatedExpr = LetAT
            "id"
            idType
            (AbsT [("x", TVar (R "a"))] (VarT "x" (TVar (R "a"))))
            (AppT (VarT "id" (bool `fn` bool)) (ConT true))
            bool
      infersType env expr (TCon "Bool" [])
      inferAndZonk env expr annotatedExpr
    it "simultaneous let definitions" $ do
      -- let x = y
      --     y = True
      --  in x
      let expr = Let [("x", Var "y"), ("y", Con true)] (Var "x")
      let annotatedExpr =
            LetT [("x", VarT "y" bool), ("y", ConT true)] (VarT "x" bool) bool
      infersType env expr bool
      inferAndZonk env expr annotatedExpr
    it "case expressions with variable patterns" $ do
      -- case True of
      --   x -> Zero
      let expr          = Case (Con true) [Alt (VarPat "x") (Con zero)]
      let annotatedExpr = CaseT (ConT true) [AltT (VarPat "x") (ConT zero)] nat
      infersType env expr (TCon "Nat" [])
      inferAndZonk env expr annotatedExpr
    it "case expressions that use bound variables" $ do
      -- case True of
      --   False -> False
      --   x -> x
      let expr = Case
            (Con true)
            [Alt (ConsPat false []) (Con false), Alt (VarPat "x") (Var "x")]
      let annotatedExpr = CaseT
            (ConT true)
            [ AltT (ConsPat false []) (ConT false)
            , AltT (VarPat "x")       (VarT "x" bool)
            ]
            bool
      infersType env expr bool
      inferAndZonk env expr annotatedExpr
    it "case expressions with wildcard patterns" $ do
      -- case True of
      --   _ -> False
      let expr          = Case (Con true) [Alt WildPat (Con false)]
      let annotatedExpr = CaseT (ConT true) [AltT WildPat (ConT false)] bool
      infersType env expr bool
      inferAndZonk env expr annotatedExpr
    it "case expressions with a mixture of patterns" $ do
      -- case True of
      --   True -> False
      --   x -> True
      let expr = Case
            (Con true)
            [Alt (ConsPat true []) (Con false), Alt (VarPat "x") (Con true)]
      let annotatedExpr = CaseT
            (ConT true)
            [AltT (ConsPat true []) (ConT false), AltT (VarPat "x") (ConT true)]
            bool
      infersType env expr bool
      inferAndZonk env expr annotatedExpr
    it "creating an instance of a parameterised type" $ do
      -- data Pair a b = MkPair a b
      -- MkPair False Zero
      let expr = App (App (Var "MkPair") (Con false)) (Con zero)
      infersType env expr (pair bool nat)
    it "deconstructing a parameterised type with a case expression (Wrap)" $ do
      -- data Wrap a = MkWrap a
      -- let x = MkWrap True
      --  in case x of
      --       MkWrap y -> y
      let x = App (Var "MkWrap") (Con true)
      let expr = Let
            [("x", x)]
            (Case (Var "x") [Alt (ConsPat mkwrap [VarPat "y"]) (Var "y")])
      infersType env expr bool
    it "deconstructing a parameterised type with a case expression (Pair)" $ do
      -- data Pair a b = MkPair a b
      -- let x = MkPair False Zero
      --  in case x of
      --       MkPair y z -> MkWrap y
      --       w -> MkWrap False
      let x    = App (App (Var "MkPair") (Con false)) (Con zero)
          expr = Let
            [("x", x)]
            (Case
              (Var "x")
              [ Alt (ConsPat mkpair [VarPat "y", VarPat "z"])
                    (App (Var "MkWrap") (Var "y"))
              , Alt (VarPat "w") (App (Var "MkWrap") (Con false))
              ]
            )
      infersType env expr (wrap bool)
    it "an expression hole" $ do
      -- let x = ?foo
      --  in True
      let expr = Let [("x", Hole "foo")] (Con true)
      infersType env expr bool
      -- let not b = case b of
      --               True -> False
      --               False -> True
      --  in not ?foo
      let expr' = Let
            [ ( "not"
              , Abs
                ["b"]
                (Case
                  (Var "b")
                  [ Alt (ConsPat true [])  (Con false)
                  , Alt (ConsPat false []) (Con true)
                  ]
                )
              )
            ]
            (App (Var "not") (Hole "foo"))
      infersType env expr' bool
    it "a tuple" $ do
      -- (True, False, Zero)
      let expr = TupleLit [Con true, Con false, Con zero]
      infersType env expr (mkTupleType [bool, bool, nat])
    it "a list" $ do
      -- [True, False]
      let expr = ListLit [Con true, Con false]
      infersType env expr (list bool)
    it "an integer literal" $ do
      let expr = IntLit 6
      infersType env expr TInt
    it "a string literal" $ do
      let expr = StringLit "Hello" [(Con true, " and "), (Con false, "")]
      infersType env expr TString

infersType :: Env -> Exp -> Type -> Expectation
infersType env expr expectedType =
  let ((_, t, constraints), touchables) = run (generate env expr)
  in  case solveC touchables constraints of
        Left  err     -> expectationFailure $ printError err
        Right (cs, s) -> do
          cs `shouldBe` mempty
          sub s t `shouldBe` expectedType

-- TODO: use this
infersError :: Env -> Exp -> Expectation
infersError env expr =
  let ((_, _, constraints), touchables) = run (generate env expr)
  in  case solveC touchables constraints of
        Left  _ -> pure ()
        Right _ -> expectationFailure "Expected type error but was successful"

inferAndZonk :: Env -> Exp -> ExpT -> Expectation
inferAndZonk env expr expectedExpr =
  let ((expr', _, constraints), touchables) = run (generate env expr)
  in  case solveC touchables constraints of
        Left err ->
          expectationFailure $ "Expected Right, found Left " <> show err
        Right (cs, s) -> do
          cs `shouldBe` mempty
          sub s expr' `shouldBe` expectedExpr

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

genUpperString :: H.Gen String
genUpperString = do
  c  <- Gen.upper
  cs <- Gen.list (Range.linear 0 10) Gen.alphaNum
  pure (c : cs)

genName :: H.Gen Name
genName = Gen.choice [Local <$> genRawName]

genModuleName :: H.Gen ModuleName
genModuleName = ModuleName <$> Gen.list (Range.linear 1 3) genUpperString

genRawName :: H.Gen RawName
genRawName = Name <$> Gen.list (Range.linear 1 5) Gen.alphaNum

genUVarName :: H.Gen Name
genUVarName = Local . Name <$> Gen.list (Range.linear 1 5) Gen.digit

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

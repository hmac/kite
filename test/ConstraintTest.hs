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

import           Util
import           Data.Name                      ( RawName(..) )
import           Constraint
import           Constraint.Solve               ( solveC
                                                , Error(..)
                                                )
import           Constraint.Generate.M          ( run )
import           Constraint.Generate            ( generate
                                                , Exp(..)
                                                , ExpT(..)
                                                , Alt(..)
                                                , AltT(..)
                                                , Pat(..)
                                                , Con(..)
                                                , Scheme(..)
                                                , Env
                                                )
import           Constraint.Generate.Bind
import           Constraint.Print

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

  describe "Constraint generation and solving combined" $ do
    it "solves simple function applications" $ do
      -- (\x -> x) True
      let expr = App (Abs "x" (Var "x")) (Con true)
      infersType env expr bool
    it "solves multi-arg function applications" $ do
      -- (\x y -> x) True (Suc Zero)
      let constfun = Abs "x" (Abs "y" (Var "x"))
      let expr     = App (App constfun (Con true)) (App (Con suc) (Con zero))
      infersType env expr bool
      let annotatedExpr = AppT
            (AppT (AbsT "x" bool (AbsT "y" nat (VarT "x" bool))) (ConT true))
            (AppT (ConT (C "Suc")) (ConT (C "Zero")))
      inferAndZonk env expr annotatedExpr
    it "solves compound lets" $ do
      -- let x = True
      --     id y = y
      --  in id x
      let expr = Let
            "x"
            (Con true)
            (Let "id" (Abs "y" (Var "y")) (App (Var "id") (Var "x")))
      let annotatedExpr = LetT
            "x"
            (ConT true)
            (LetT "id"
                  (AbsT "y" bool (VarT "y" bool))
                  (AppT (VarT "id" (bool `fn` bool)) (VarT "x" bool))
                  bool
            )
            bool
      infersType env expr (TCon "Bool" [])
      inferAndZonk env expr annotatedExpr
    it "solves simple case expressions" $ do
      -- case True of
      --   True -> False
      --   False -> True
      let expr = Case
            (Con true)
            [Alt (ConPat true []) (Con false), Alt (ConPat false []) (Con true)]
      let annotatedExpr = CaseT
            (ConT true)
            [ AltT (ConPat true [])  (ConT false)
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
            [ Alt (ConPat true []) (Let "id" idfun (App (Var "id") (Con true)))
            , Alt (ConPat false []) (Con true)
            ]
      let annotatedExpr = CaseT
            (ConT true)
            [ AltT
              (ConPat true [])
              (LetT "id"
                    (AbsT "y" bool (VarT "y" bool))
                    (AppT (VarT "id" (bool `fn` bool)) (ConT true))
                    bool
              )
            , AltT (ConPat false []) (ConT true)
            ]
            bool
      infersType env expr (TCon "Bool" [])
      inferAndZonk env expr annotatedExpr
    it "solves expressions with annotated lets" $ do
      -- let id : a -> a
      --     id = \x -> x
      --  in id True
      let idType = Forall [R "a"] CNil (TVar (R "a") `fn` TVar (R "a"))
      let expr =
            LetA "id" idType (Abs "x" (Var "x")) (App (Var "id") (Con true))
      let annotatedExpr = LetAT
            "id"
            idType
            (AbsT "x" (TVar (R "a")) (VarT "x" (TVar (R "a"))))
            (AppT (VarT "id" (bool `fn` bool)) (ConT true))
            bool
      infersType env expr (TCon "Bool" [])
      inferAndZonk env expr annotatedExpr
    it "solves case expressions with variable patterns" $ do
      -- case True of
      --   x -> Zero
      let expr = Case (Con true) [Alt (VarPat "x") (Con (C "Zero"))]
      let annotatedExpr =
            CaseT (ConT true) [AltT (VarPat "x") (ConT (C "Zero"))] nat
      infersType env expr (TCon "Nat" [])
      inferAndZonk env expr annotatedExpr
    it "solves case expressions that use bound variables" $ do
      -- case True of
      --   False -> False
      --   x -> x
      let expr = Case
            (Con true)
            [Alt (ConPat false []) (Con false), Alt (VarPat "x") (Var "x")]
      let annotatedExpr = CaseT
            (ConT true)
            [ AltT (ConPat false []) (ConT false)
            , AltT (VarPat "x")      (VarT "x" bool)
            ]
            bool
      infersType env expr bool
      inferAndZonk env expr annotatedExpr
    it "solves case expressions with wildcard patterns" $ do
      -- case True of
      --   _ -> False
      let expr          = Case (Con true) [Alt WildPat (Con false)]
      let annotatedExpr = CaseT (ConT true) [AltT WildPat (ConT false)] bool
      infersType env expr bool
      inferAndZonk env expr annotatedExpr
    it "solves case expressions with a mixture of patterns" $ do
      -- case True of
      --   True -> False
      --   x -> True
      let expr = Case
            (Con true)
            [Alt (ConPat true []) (Con false), Alt (VarPat "x") (Con true)]
      let annotatedExpr = CaseT
            (ConT true)
            [AltT (ConPat true []) (ConT false), AltT (VarPat "x") (ConT true)]
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
      let expr =
            Let "x" x (Case (Var "x") [Alt (ConPat mkwrap ["y"]) (Var "y")])
      infersType env expr bool
    it "deconstructing a parameterised type with a case expression (Pair)" $ do
      -- data Pair a b = MkPair a b
      -- let x = MkPair False Zero
      --  in case x of
      --       MkPair y z -> MkWrap y
      --       w -> MkWrap False
      let x    = App (App (Var "MkPair") (Con false)) (Con zero)
          expr = Let
            "x"
            x
            (Case
              (Var "x")
              [ Alt (ConPat mkpair ["y", "z"]) (App (Var "MkWrap") (Var "y"))
              , Alt (VarPat "w")               (App (Var "MkWrap") (Con false))
              ]
            )
      infersType env expr (wrap bool)
  describe "typing top level function binds" $ do
    it "types a simple unannotated function bind" $ do
      -- f = \x -> case x of
      --             True -> True
      --             False -> False
      let expr = Abs
            "x"
            (Case
              (Var "x")
              [ Alt (ConPat true [])  (Con true)
              , Alt (ConPat false []) (Con false)
              ]
            )
      let bind = Bind "f" expr Nothing
      let bindT = BindT
            (Name "f")
            (AbsT
              (Name "x")
              bool
              (CaseT
                (VarT (Name "x") bool)
                [ AltT (ConPat true [])  (ConT true)
                , AltT (ConPat false []) (ConT false)
                ]
                bool
              )
            )
            (Forall [] CNil (TCon (Name "->") [bool, bool]))
      let (res, _) = run (generateBind env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (bind', _) -> bind' `shouldBe` bindT
    it "types a simple annotated function bind" $ do
      -- f : Bool -> Bool
      -- f = \x -> case x of
      --             True -> True
      --             False -> False
      let expr = Abs
            "x"
            (Case
              (Var "x")
              [ Alt (ConPat true [])  (Con true)
              , Alt (ConPat false []) (Con false)
              ]
            )
      let bind = Bind "f" expr (Just (Forall [] CNil (bool `fn` bool)))
      let bindT = BindT
            (Name "f")
            (AbsT
              (Name "x")
              bool
              (CaseT
                (VarT (Name "x") bool)
                [ AltT (ConPat true [])  (ConT true)
                , AltT (ConPat false []) (ConT false)
                ]
                bool
              )
            )
            (Forall [] CNil (TCon (Name "->") [bool, bool]))
      let (res, _) = run (generateBind env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (bind', _) -> bind' `shouldBe` bindT

infersType :: Env -> Exp -> Type -> Expectation
infersType env expr expectedType =
  let ((_, TVar t, constraints), touchables) = run (generate env expr)
  in  case solveC touchables constraints of
        Left  err     -> expectationFailure $ printError err
        Right (cs, s) -> do
          lookup t s `shouldBe` Just expectedType
          cs `shouldBe` mempty

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

genName :: H.Gen RawName
genName = Name <$> Gen.list (Range.linear 1 5) Gen.alphaNum

genUVarName :: H.Gen RawName
genUVarName = Name <$> Gen.list (Range.linear 1 5) Gen.digit

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

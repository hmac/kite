module Test.Constraint.Bind
  ( test
  )
where

import           Test.Hspec

import qualified Data.Map.Strict               as Map

import           Util
import           Data.Name                      ( RawName(..) )
import           Constraint
import           Constraint.Expr
import           Constraint.Solve               ( solveC
                                                , Error(..)
                                                )
import           Constraint.Generate.M          ( run )
import           Constraint.Generate            ( generate
                                                , mkTupleType
                                                , Env
                                                )
import           Constraint.Generate.Pattern
import           Constraint.Generate.Bind
import           Constraint.Print

-- Tests the constraint solver

test :: Spec
test = do
  let
    bool = TCon "Bool" []
    nat  = TCon "Nat" []
    list a = TCon "List" [a]
    tuple2 a b = TCon "Tuple2" [a, b]
    pair a b = TCon "Pair" [a, b]
    wrap a = TCon "Wrap" [a]
    either a b = TCon "Either" [a, b]

    true   = C "True"
    false  = C "False"
    zero   = C "Zero"
    suc    = C "Suc"
    cons   = C "::"
    mkpair = C "MkPair"
    mkwrap = C "MkWrap"
    left   = C "Left"
    right  = C "Right"

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
      , ( "Left"
        , Forall [R "a", R "b"]
                 CNil
                 (TVar (R "a") `fn` either (TVar (R "a")) (TVar (R "b")))
        )
      , ( "Right"
        , Forall [R "a", R "b"]
                 CNil
                 (TVar (R "b") `fn` either (TVar (R "a")) (TVar (R "b")))
        )
      ]

  describe "typing top level function binds" $ do
    it "a simple unannotated function bind" $ do
      -- f x = case x of
      --         True -> True
      --         False -> False
      let pat = VarPat "x"
      let expr = Case
            (Var "x")
            [ Alt (SConPat true [])  (Con true)
            , Alt (SConPat false []) (Con false)
            ]

      let bind = Bind "f" Nothing [(pat, expr)]
      let bindT = BindT
            (Name "f")
            [ ( pat
              , CaseT
                (VarT (Name "x") bool)
                [ AltT (SConPat true [])  (ConT true)
                , AltT (SConPat false []) (ConT false)
                ]
                bool
              )
            ]
            (Forall [] CNil (TCon (Name "->") [bool, bool]))
      let (res, _) = run (generateBind env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (bind', _) -> do
          bind' `shouldBe` bindT
    it "a bind with pattern matching" $ do
      -- f True = True
      -- f False = False
      let body = [(ConPat true [], Con true), (ConPat false [], Con false)]
      let bind = Bind "f" Nothing body
      let bindT = BindT
            (Name "f")
            [(ConPat true [], ConT true), (ConPat false [], ConT false)]
            (Forall [] CNil (TCon (Name "->") [bool, bool]))
      let (res, _) = run (generateBind env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (bind', _) -> bind' `shouldBe` bindT
    it "list patterns" $ do
      -- f [] = False
      -- f (x :: xs) = True
      let body =
            [ (ListPat []                           , Con false)
            , (ConPat cons [VarPat "x", VarPat "xs"], Con true)
            ]
      let bind = Bind "f" Nothing body
      let
        bindT = BindT
          (Name "f")
          [ (ListPat []                           , ConT false)
          , (ConPat cons [VarPat "x", VarPat "xs"], ConT true)
          ]
          (Forall [R "$R12"]
                  CNil
                  (TCon (Name "->") [list (TVar (R "$R12")), bool])
          )
      let (res, _) = run (generateBind env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (bind', _) -> bind' `shouldBe` bindT
    it "tuple patterns" $ do
      -- f (True, y) = y
      -- f (x, Zero) = Zero
      let body =
            [ (TuplePat [ConPat true [], VarPat "y"], Var "y")
            , (TuplePat [VarPat "x", ConPat zero []], Con zero)
            ]
      let bind = Bind "f" Nothing body
      let bindT = BindT
            "f"
            [ (TuplePat [ConPat true [], VarPat "y"], VarT "y" nat)
            , (TuplePat [VarPat "x", ConPat zero []], ConT zero)
            ]
            (Forall [] CNil (tuple2 bool nat `fn` nat))
      let (res, _) = run (generateBind env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (bind', _) -> bind' `shouldBe` bindT
    it "a simple annotated function bind" $ do
      -- f : Bool -> Bool
      -- f x = case x of
      --         True -> True
      --         False -> False
      let pat = VarPat "x"
      let expr = Case
            (Var "x")
            [ Alt (SConPat true [])  (Con true)
            , Alt (SConPat false []) (Con false)
            ]

      let bind =
            Bind "f" (Just (Forall [] CNil (bool `fn` bool))) [(pat, expr)]
      let bindT = BindT
            (Name "f")
            [ ( VarPat "x"
              , CaseT
                (VarT (Name "x") bool)
                [ AltT (SConPat true [])  (ConT true)
                , AltT (SConPat false []) (ConT false)
                ]
                bool
              )
            ]
            (Forall [] CNil (TCon (Name "->") [bool, bool]))
      let (res, _) = run (generateBind env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (bind', _) -> bind' `shouldBe` bindT
    it "constructors with arguments" $ do
      -- f (MkPair x Zero) = x
      -- f _ = False
      let body =
            [ (ConPat mkpair [VarPat "x", ConPat zero []], Var "x")
            , (WildPat, Con false)
            ]
      let bind = Bind "f" Nothing body
      let bindT = BindT
            "f"
            [ (ConPat mkpair [VarPat "x", ConPat zero []], VarT "x" bool)
            , (WildPat, ConT false)
            ]
            (Forall [] CNil (pair bool nat `fn` bool))
      let (res, _) = run (generateBind env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (bind', _) -> bind' `shouldBe` bindT
    it "Either Bool Nat" $ do
      -- f (Left False) = Zero
      -- f (Left _)     = Suc Zero
      -- f (Right n)    = n
      let body =
            [ (ConPat left [ConPat false []], Con zero)
            , (ConPat left [WildPat]        , App (Con suc) (Con zero))
            , (ConPat right [VarPat "n"]    , Var "n")
            ]
      let bind = Bind "f" Nothing body
      let bindT = BindT
            "f"
            [ (ConPat left [ConPat false []], ConT zero)
            , (ConPat left [WildPat]        , AppT (ConT suc) (ConT zero))
            , (ConPat right [VarPat "n"]    , VarT "n" nat)
            ]
            (Forall [] CNil (either bool nat `fn` nat))
      let (res, _) = run (generateBind env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (bind', _) -> bind' `shouldBe` bindT
    describe "expected type failures" $ do
      it "mismatched constructors in pattern" $ do
        -- f True = True
        -- f Zero = False
        let body = [(ConPat true [], Con true), (ConPat zero [], Con false)]
        let bind = Bind "f" Nothing body
        infersError env bind
      it "type mismatch between list pattern and boolean" $ do
        -- f [] = False
        -- f x = x
        let body = [(ListPat [], Con false), (VarPat "x", Var "x")]
        let bind = Bind "f" Nothing body
        infersError env bind
      it "type mismatch in tuple pattern" $ do
        -- f (True, y) = y
        -- f (Zero, x) = x
        let body =
              [ (TuplePat [ConPat true [], VarPat "y"], Var "y")
              , (TuplePat [ConPat zero [], VarPat "x"], Var "x")
              ]
        let bind = Bind "f" Nothing body
        infersError env bind

infersType :: Env -> Exp -> Type -> Expectation
infersType env expr expectedType =
  let ((_, t, constraints), touchables) = run (generate env expr)
  in  case solveC touchables constraints of
        Left  err     -> expectationFailure $ printError err
        Right (cs, s) -> do
          cs `shouldBe` mempty
          sub s t `shouldBe` expectedType

infersError :: Env -> Bind -> Expectation
infersError env bind =
  let (res, _) = run (generateBind env bind)
  in  case res of
        Left  err -> pure ()
        Right _   -> expectationFailure "Expected type error but was successful"

inferAndZonk :: Env -> Exp -> ExpT -> Expectation
inferAndZonk env expr expectedExpr =
  let ((expr', _, constraints), touchables) = run (generate env expr)
  in  case solveC touchables constraints of
        Left err ->
          expectationFailure $ "Expected Right, found Left " <> show err
        Right (cs, s) -> do
          cs `shouldBe` mempty
          sub s expr' `shouldBe` expectedExpr

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

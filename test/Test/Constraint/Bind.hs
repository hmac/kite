module Test.Constraint.Bind
  ( test
  )
where

import           Test.Hspec

import qualified Data.Map.Strict               as Map

import           Constraint
import           Constraint.Expr
import           Constraint.Generate.M          ( run
                                                , TypeEnv
                                                )
import           Constraint.Generate.Bind
import           Syn                            ( Pattern_(..) )
import           Util

-- Tests the constraint solver

test :: Spec
test = do
  let
    bool = TCon "Bool"
    nat  = TCon "Nat"
    tuple2 a b = mkTupleType [a, b]
    pair a = TApp (TApp (TCon "Pair") a)
    either a = TApp (TApp (TCon "Either") a)
    maybe   = TApp (TCon "Maybe")

    true    = "True"
    false   = "False"
    zero    = "Zero"
    suc     = "Suc"
    cons    = "::"
    mkpair  = "MkPair"
    left    = "Left"
    right   = "Right"
    just    = "Just"
    nothing = "Nothing"

    env     = Map.fromList
      [ ("True" , Forall [] CNil (TCon "Bool"))
      , ("False", Forall [] CNil (TCon "Bool"))
      , ("Zero" , Forall [] CNil (TCon "Nat"))
      , ("Suc"  , Forall [] CNil (TCon "Nat" `fn` TCon "Nat"))
      , ( "::"
        , Forall
          [R "a"]
          CNil
          (TVar (R "a") `fn` list (TVar (R "a")) `fn` list (TVar (R "a")))
        )
      , ( "MkPair"
        , Forall
          [R "a", R "b"]
          CNil
          (    TVar (R "a")
          `fn` (TVar (R "b") `fn` pair (TVar (R "a")) (TVar (R "b")))
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
      , ("Just", Forall [R "a"] CNil (TVar (R "a") `fn` maybe (TVar (R "a"))))
      , ("Nothing", Forall [R "a"] CNil (maybe (TVar (R "a"))))
      ]

  describe "typing top level function binds" $ do
    it "a simple unannotated function bind" $ do
      -- f x = case x of
      --         True -> True
      --         False -> False
      let pat = VarPat "x"
      let expr = Case
            (Var "x")
            [(ConsPat true [], Con true), (ConsPat false [], Con false)]

      let bind = Bind "f" Nothing [([pat], expr)]
      let bindT = BindT
            "f"
            [ ( [pat]
              , CaseT
                (VarT "x" bool)
                [ AltT (ConsPat true [])  (ConT true)
                , AltT (ConsPat false []) (ConT false)
                ]
                bool
              )
            ]
            (Forall [] CNil (bool `fn` bool))
      let (res, _) = run (generateBind mempty env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (_, bind') -> bind' `shouldBe` bindT
    it "a bind with pattern matching" $ do
      -- f True = True
      -- f False = False
      let body =
            [([ConsPat true []], Con true), ([ConsPat false []], Con false)]
      let bind = Bind "f" Nothing body
      let bindT = BindT
            "f"
            [([ConsPat true []], ConT true), ([ConsPat false []], ConT false)]
            (Forall [] CNil (bool `fn` bool))
      let (res, _) = run (generateBind mempty env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (_, bind') -> bind' `shouldBe` bindT
    it "list patterns" $ do
      -- f [] = False
      -- f (x :: xs) = True
      let body =
            [ ([ListPat []]                            , Con false)
            , ([ConsPat cons [VarPat "x", VarPat "xs"]], Con true)
            ]
      let bind = Bind "f" Nothing body
      let bindT = BindT
            "f"
            [ ([ListPat []]                            , ConT false)
            , ([ConsPat cons [VarPat "x", VarPat "xs"]], ConT true)
            ]
            (Forall [R "$R11"] CNil (list (TVar (R "$R11")) `fn` bool))
      let (res, _) = run (generateBind mempty env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (_, bind') -> bind' `shouldBe` bindT
    it "a recursive function" $ do
      -- f [] = True
      -- f (x :: xs) = f xs
      let body =
            [ ([ListPat []], Con true)
            , ( [ConsPat cons [VarPat "x", VarPat "xs"]]
              , App (Var "f") (Var "xs")
              )
            ]
      let bind        = Bind "f" Nothing body
      let overallType = list (TVar (R "$R12")) `fn` bool
      let bindT = BindT
            "f"
            [ ([ListPat []], ConT true)
            , ( [ConsPat cons [VarPat "x", VarPat "xs"]]
              , AppT (VarT "f" overallType) (VarT "xs" (list (TVar (R "$R12"))))
              )
            ]
            (Forall [R "$R12"] CNil overallType)
      let (res, _) = run (generateBind mempty env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (_, bind') -> bind' `shouldBe` bindT
    it "tuple patterns" $ do
      -- f (True, y) = y
      -- f (x, Zero) = Zero
      let body =
            [ ([TuplePat [ConsPat true [], VarPat "y"]], Var "y")
            , ([TuplePat [VarPat "x", ConsPat zero []]], Con zero)
            ]
      let bind = Bind "f" Nothing body
      let bindT = BindT
            "f"
            [ ([TuplePat [ConsPat true [], VarPat "y"]], VarT "y" nat)
            , ([TuplePat [VarPat "x", ConsPat zero []]], ConT zero)
            ]
            (Forall [] CNil (tuple2 bool nat `fn` nat))
      let (res, _) = run (generateBind mempty env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (_, bind') -> bind' `shouldBe` bindT
    it "a simple annotated function bind" $ do
      -- f : Bool -> Bool
      -- f x = case x of
      --         True -> True
      --         False -> False
      let pat = VarPat "x"
      let expr = Case
            (Var "x")
            [(ConsPat true [], Con true), (ConsPat false [], Con false)]

      let bind =
            Bind "f" (Just (Forall [] CNil (bool `fn` bool))) [([pat], expr)]
      let bindT = BindT
            "f"
            [ ( [VarPat "x"]
              , CaseT
                (VarT "x" bool)
                [ AltT (ConsPat true [])  (ConT true)
                , AltT (ConsPat false []) (ConT false)
                ]
                bool
              )
            ]
            (Forall [] CNil (bool `fn` bool))
      let (res, _) = run (generateBind mempty env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (_, bind') -> bind' `shouldBe` bindT
    it "constructors with arguments" $ do
      -- f (MkPair x Zero) = x
      -- f _ = False
      let body =
            [ ([ConsPat mkpair [VarPat "x", ConsPat zero []]], Var "x")
            , ([WildPat], Con false)
            ]
      let bind = Bind "f" Nothing body
      let bindT = BindT
            "f"
            [ ([ConsPat mkpair [VarPat "x", ConsPat zero []]], VarT "x" bool)
            , ([WildPat], ConT false)
            ]
            (Forall [] CNil (pair bool nat `fn` bool))
      let (res, _) = run (generateBind mempty env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (_, bind') -> bind' `shouldBe` bindT
    it "Either Bool Nat" $ do
      -- f (Left False) = Zero
      -- f (Left _)     = Suc Zero
      -- f (Right n)    = n
      let body =
            [ ([ConsPat left [ConsPat false []]], Con zero)
            , ([ConsPat left [WildPat]]         , App (Con suc) (Con zero))
            , ([ConsPat right [VarPat "n"]]     , Var "n")
            ]
      let bind = Bind "f" Nothing body
      let bindT = BindT
            "f"
            [ ([ConsPat left [ConsPat false []]], ConT zero)
            , ([ConsPat left [WildPat]]         , AppT (ConT suc) (ConT zero))
            , ([ConsPat right [VarPat "n"]]     , VarT "n" nat)
            ]
            (Forall [] CNil (either bool nat `fn` nat))
      let (res, _) = run (generateBind mempty env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (_, bind') -> bind' `shouldBe` bindT
    it "Maybe Bool" $ do
      -- f (Just b) = b
      -- f Nothing  = False
      let body =
            [ ([ConsPat just [VarPat "b"]], Var "b")
            , ([ConsPat nothing []]       , Con false)
            ]
      let bind = Bind "f" Nothing body
      let bindT = BindT
            "f"
            [ ([ConsPat just [VarPat "b"]], VarT "b" bool)
            , ([ConsPat nothing []]       , ConT false)
            ]
            (Forall [] CNil (maybe bool `fn` bool))
      let (res, _) = run (generateBind mempty env bind)
      case res of
        Left  err        -> expectationFailure (show err)
        Right (_, bind') -> bind' `shouldBe` bindT
    describe "expected type failures" $ do
      it "mismatched constructors in pattern" $ do
        -- f True = True
        -- f Zero = False
        let body =
              [([ConsPat true []], Con true), ([ConsPat zero []], Con false)]
        let bind = Bind "f" Nothing body
        infersError env bind
      it "type mismatch between list pattern and boolean" $ do
        -- f [] = False
        -- f x = x
        let body = [([ListPat []], Con false), ([VarPat "x"], Var "x")]
        let bind = Bind "f" Nothing body
        infersError env bind
      it "type mismatch in tuple pattern" $ do
        -- f (True, y) = y
        -- f (Zero, x) = x
        let body =
              [ ([TuplePat [ConsPat true [], VarPat "y"]], Var "y")
              , ([TuplePat [ConsPat zero [], VarPat "x"]], Var "x")
              ]
        let bind = Bind "f" Nothing body
        infersError env bind
      it "type mismatch between function and annotation" $ do
        -- id : a -> a
        -- id x = 5
        let body       = [([VarPat "x"], IntLit 5)]
        let annotation = Forall [R "a"] CNil (TVar (R "a") `fn` TVar (R "a"))
        let bind       = Bind "id" (Just annotation) body
        infersError env bind
      it "different numbers of patterns in equations" $ do
        -- f True = True
        -- f True False = True
        let body =
              [ ([ConsPat true []]                  , Con true)
              , ([ConsPat true [], ConsPat false []], Con true)
              ]
        let bind = Bind "f" Nothing body
        infersError env bind

infersError :: TypeEnv -> Bind -> Expectation
infersError env bind = case run (generateBind mempty env bind) of
  (Right (_env, bindt), _) ->
    expectationFailure
      $  "Expected type error but was successful: \n"
      <> pShow bindt
  (Left _, _) -> pure ()

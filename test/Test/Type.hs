module Test.Type where

import           Test.Hspec
import           Type
import           Data.Name
import           Control.Monad.Trans.State.Strict
                                                ( put )
import           Control.Monad.Trans.Class      ( lift )

test :: Spec
test = do
  describe "Typechecking Data.Semigroup.Semigroup" $ do
    let
        -- This passes
        -- D : { field : Bool } -> D a
        -- f : a -> D a -> a
        -- f = x (D d) -> x
      ctx1 =
        [ Var
            (Free "D")
            (Forall
              (U 0 "a")
              (Fn (TRecord [("field", bool)]) (TCon "D" [UType (U 0 "a")]))
            )
        ]
      funType1 = Forall
        (U 1 "a")
        (Fn (UType (U 1 "a"))
            (Fn (TCon "D" [UType (U 1 "a")]) (UType (U 1 "a")))
        )
      funBody1 = MCase
        [ ( [VarPat (Free "x"), ConsPat (Free "D") [VarPat (Free "d")]]
          , VarExp (Free "x")
          )
        ]
      -- This fails
      -- D : { field : a } -> D a
      -- f : a -> D a -> a
      -- f = x (D d) -> x
      ctx2 =
        [ Var
            (Free "D")
            (Forall
              (U 0 "a")
              (Fn (TRecord [("field", UType (U 0 "a"))])
                  (TCon "D" [UType (U 0 "a")])
              )
            )
        ]
      funType2 = Forall
        (U 1 "a")
        (Fn (UType (U 1 "a"))
            (Fn (TCon "D" [UType (U 1 "a")]) (UType (U 1 "a")))
        )
      funBody2 = MCase
        [ ( [VarPat (Free "x"), ConsPat (Free "D") [VarPat (Free "d")]]
          , VarExp (Free "x")
          )
        ]
      -- This ???
      -- data D a = D (a -> a)
      -- [D : (a -> a) -> D a]
      -- f : D a -> (a -> a)
      -- f = (D f) -> f
      ctx3 =
        [ Var
            (Free "D")
            (Forall
              (U 0 "a")
              (Fn (Fn (UType (U 0 "a")) (UType (U 0 "a")))
                  (TCon "D" [UType (U 0 "a")])
              )
            )
        ]
      funType3 = Forall
        (U 1 "a")
        (Fn (TCon "D" [UType (U 1 "a")])
            (Fn (UType (U 1 "a")) (UType (U 1 "a")))
        )
      funBody3 =
        MCase [([ConsPat (Free "D") [VarPat (Free "x")]], VarExp (Free "x"))]
      -- This fails
      -- D : { field : a } -> D a
      -- f : D a -> a -> a
      -- f = (D d) x -> x
      ctx4 =
        [ Var
            (Free "D")
            (Forall
              (U 0 "a")
              (Fn (TRecord [("field", UType (U 0 "a"))])
                  (TCon "D" [UType (U 0 "a")])
              )
            )
        ]
      funType4 = Forall
        (U 1 "a")
        (Fn (TCon "D" [UType (U 1 "a")])
            (Fn (UType (U 1 "a")) (UType (U 1 "a")))
        )
      funBody4 = MCase
        [ ( [ConsPat (Free "D") [VarPat (Free "d")], VarPat (Free "x")]
          , VarExp (Free "x")
          )
        ]

    -- it "typechecks successfully (1)" $ do
    --   -- Ensure 0 and 1 are not used for new variable indices
    --   let r = do
    --         lift $ lift $ put 2
    --         _ <- check ctx funBody1 funType1
    --         pure ()
    --   runTypeM ctx1 r `shouldBe` Right ()
    -- it "typechecks successfully (2)" $ do
    --   let r = do
    --         lift $ lift $ put 2
    --         _ <- check ctx2 funBody2 funType2
    --         pure ()
    --   runTypeM ctx2 r `shouldBe` Right ()
    it "typechecks successfully (3)" $ do
      let r = do
            lift $ lift $ put 2
            _ <- check ctx3 funBody3 funType3
            pure ()
          env = defaultTypeEnv { envCtx = ctx3 }
      runTypeM env r `shouldBe` Right ()
    -- it "typechecks successfully (4)" $ do
    --   let r = do
    --         lift $ lift $ put 2
    --         _ <- check mempty funBody4 funType4
    --         pure ()
    --   runTypeM ctx4 r `shouldBe` Right ()

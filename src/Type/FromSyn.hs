{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Type.FromSyn where

-- Convert Syn to Type.Exp, ready for typechecking.
-- This just involves converting 'Syn.Type' to 'Type.Type.Type'.
-- It needs a 'TypeM' context to generate fresh type variables.

import           Util

import           Data.Generics.Product
import           Data.Name                      ( Name
                                                , prim
                                                , toString
                                                )
import qualified Data.Set                      as Set
import           Type                           ( Exp )
import qualified Type                          as T
import           Type.DSL                       ( fn
                                                , forAll
                                                , ifn
                                                , tapp
                                                , tcon
                                                , trecord
                                                , ttuple
                                                , u_
                                                )
import qualified Type.Type                     as T

import qualified Canonical                     as Can
import qualified Syn                           as S

-- | Traverse the expression, converting all types from 'Can.Type' to
-- 'Type.Type.Type'. This also explicitly quantifies any types containing
-- free type variables.
fromSyn :: Can.Exp -> T.TypeM Exp
fromSyn = param @0 $ \t -> quantify (Set.toList (S.ftv t)) t

-- Explicitly quantify all type variables, then convert the whole thing to a
-- T.Type.
-- a -> b -> c ===> forall u0 u1 u2. u0 -> u1 -> u2
quantify :: [Name] -> Can.Type -> T.TypeM T.Type
quantify vars t = do
  uMap <- mapM (\v -> (v, ) <$> T.newU v) vars
  t'   <- convertType uMap t
  pure $ foldr (forAll . snd) t' uMap

-- | Convert 'Can.Type' to 'Type.Type.Type'.
-- Mostly this just swaps one constructor for another.
-- It also flattens type applications into spine form, e.g.
--   ((f x) y) ==> f x y
-- Type variables are replaced by fresh UTypes.
convertType :: [(Name, T.U)] -> Can.Type -> T.TypeM T.Type
convertType uVarCtx = \case
  S.TyBool   -> pure T.bool
  S.TyInt    -> pure T.int
  S.TyString -> pure T.string
  S.TyChar   -> pure T.char
  S.TyUnit   -> pure T.unit
  S.TyHole _ ->
    T.throwError $ T.TodoError "Type.fromSyn: holes in types not implemented"
  S.TyFun  a b -> fn <$> convertType uVarCtx a <*> convertType uVarCtx b
  S.TyIFun a b -> ifn <$> convertType uVarCtx a <*> convertType uVarCtx b
  S.TyTuple as -> ttuple <$> mapM (convertType uVarCtx) as
  S.TyVar   v  -> case lookup v uVarCtx of
    Just u  -> pure $ u_ u
    Nothing -> T.throwError $ T.UnknownVariable v
  S.TyCon c   -> pure $ tcon c []
  -- Flatten type applications into spine form, so the head of every TApp is
  -- never a TApp. This is an invariant required by the typechecker.
  S.TyApp a b -> do
    b' <- convertType uVarCtx b
    a' <- convertType uVarCtx a
    pure $ case a' of
      T.TCon c args -> tcon c $ args ++ [b']
      T.TApp f args -> tapp f (args <> [b'])
      T.TOther f    -> tapp f [b']
  S.TyList -> pure $ tcon (prim "List") []
  S.TyRecord fields ->
    trecord <$> mapM (secondM (convertType uVarCtx) . first toString) fields
  S.TyAlias _ _ ->
    T.throwError $ T.TodoError "convertType: type aliases not implemented"
  S.TyForall v t -> do
    u  <- T.newU v
    t' <- convertType ((v, u) : uVarCtx) t
    pure $ forAll u t'

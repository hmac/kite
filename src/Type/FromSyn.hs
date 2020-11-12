module Type.FromSyn where

-- Convert Syn to Type.Exp, ready for typechecking

import           Util

import           Data.Traversable               ( for )
import qualified Data.Set                      as Set
import           Data.String                    ( fromString )
import           Data.Name                      ( Name
                                                , toString
                                                )
import           Type                           ( Exp )
import qualified Type                          as T

import qualified Canonical                     as Can
import qualified Syn                           as S
import           AST

fromSyn :: Can.Exp -> T.TypeM Exp
fromSyn = \case
  Var s n   -> pure $ Var s (T.Free n)
  Con s n   -> pure $ Con s (T.Free n)
  Ann s e t -> Ann s <$> fromSyn e <*> convertType mempty t
  Hole s n  -> pure $ Hole s (T.Free n)
  App s a b -> App s <$> fromSyn a <*> fromSyn b
  Case s scrut alts ->
    Case s
      <$> fromSyn scrut
      <*> mapM (bimapM (pure . convertPattern) fromSyn) alts
  MCase s alts ->
    MCase s <$> mapM (bimapM (pure . map convertPattern) fromSyn) alts
  Abs s xs a -> do
    a' <- fromSyn a
    pure $ Abs s (map T.Free xs) a'
  Let s binds body -> do
    body'  <- fromSyn body
    binds' <- mapM
      (\(n, e, maybeType) -> do
        let t' = for maybeType $ \t -> quantify (Set.toList (S.ftv t)) t
        (T.Free n, , ) <$> fromSyn e <*> t'
      )
      binds

    pure $ Let s binds' body'
  UnitLit s       -> pure $ UnitLit s
  TupleLit  s es  -> TupleLit s <$> mapM fromSyn es
  ListLit   s es  -> ListLit s <$> mapM fromSyn es
  StringLit s str -> pure $ StringLit s str
  StringInterp s prefix comps ->
    StringInterp s prefix <$> mapM (firstM fromSyn) comps
  CharLit s c      -> pure $ CharLit s c
  IntLit  s i      -> pure $ IntLit s i
  BoolLit s b      -> pure $ BoolLit s b
  Record  s r      -> Record s <$> mapM (secondM fromSyn) r
  Project s r f    -> Project s <$> fromSyn r <*> pure f
  FCall   s n args -> FCall s n <$> mapM fromSyn args

convertPattern :: Can.Pattern -> T.Pattern
convertPattern = \case
  VarPat v          -> VarPat (T.Free v)
  ConsPat c subpats -> ConsPat (T.Free c) (map convertPattern subpats)
  TuplePat subpats  -> TuplePat (map convertPattern subpats)
  ListPat  subpats  -> ListPat (map convertPattern subpats)
  WildPat           -> WildPat
  UnitPat           -> UnitPat
  IntPat    i       -> IntPat i
  CharPat   c       -> CharPat c
  BoolPat   b       -> BoolPat b
  StringPat s       -> StringPat s

convertType :: [(Name, T.U)] -> Can.Type -> T.TypeM T.Type
convertType uVarCtx = \case
  S.TyBool   -> pure T.bool
  S.TyInt    -> pure T.int
  S.TyString -> pure T.string
  S.TyChar   -> pure T.char
  S.TyUnit   -> pure T.unit
  S.TyHole _ ->
    T.throwError $ T.TodoError "Type.fromSyn: holes in types not implemented"
  S.TyFun a b -> T.Fn <$> convertType uVarCtx a <*> convertType uVarCtx b
  S.TyTuple as ->
    let name = fromString $ "Kite.Primitive.Tuple" <> show (length as)
    in  T.TCon name <$> mapM (convertType uVarCtx) as
  S.TyVar v -> case lookup v uVarCtx of
    Just u  -> pure $ T.UType u
    Nothing -> T.throwError $ T.UnknownVariable (T.Free v)
  S.TyCon c   -> pure $ T.TCon c []
  -- Flatten type applications into spine form, so the head of every TApp is
  -- never a TApp. This is an invariant required by the typechecker.
  S.TyApp a b -> do
    b' <- convertType uVarCtx b
    a' <- convertType uVarCtx a
    pure $ case a' of
      T.TCon c args -> T.TCon c $ args ++ [b']
      T.TApp f args -> T.TApp f (args <> [b'])
      _             -> T.TApp a' [b']
  S.TyList -> pure $ T.TCon "Kite.Primitive.List" []
  S.TyRecord fields ->
    T.TRecord <$> mapM (secondM (convertType uVarCtx) . first toString) fields
  S.TyAlias _ _ ->
    T.throwError $ T.TodoError "convertType: type aliases not implemented"
  S.TyForall v t -> do
    u  <- T.newU v
    t' <- convertType ((v, u) : uVarCtx) t
    pure $ T.Forall u t'

-- Explicitly quantify all type variables, then convert the whole thing to a
-- T.Type.
quantify :: [Name] -> Can.Type -> T.TypeM T.Type
quantify vars t = do
  uMap <- mapM (\v -> (v, ) <$> T.newU v) vars
  t'   <- convertType uMap t
  pure $ foldr (T.Forall . snd) t' uMap

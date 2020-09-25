module Type.FromSyn where

-- Convert Syn to Type.Exp, ready for typechecking

import           Util

import           Data.String                    ( fromString )
import           Data.Name                      ( Name(TopLevel)
                                                , RawName(Name)
                                                , fromLocal
                                                , toString
                                                )
import           Type                           ( Exp )
import qualified Type                          as T

import qualified Canonical                     as Can
import qualified Syn                           as S

fromSyn :: Can.Exp -> T.TypeM Exp
fromSyn = \case
  S.LetA x ty e body -> do
    e' <- T.Ann <$> fromSyn e <*> convertScheme ty
    T.Let [(T.Free x, e')] <$> fromSyn body
  S.Var n   -> pure $ T.VarExp (T.Free n)
  S.Con n   -> pure $ T.Con (T.Free n)
  S.Ann e t -> T.Ann <$> fromSyn e <*> convertType mempty t
  S.Hole n  -> pure $ T.Hole (show n)
  S.App a b -> T.App <$> fromSyn a <*> fromSyn b
  S.Case scrut alts ->
    T.Case
      <$> fromSyn scrut
      <*> mapM (bimapM (pure . convertPattern) fromSyn) alts
  S.MCase alts ->
    T.MCase <$> mapM (bimapM (pure . map convertPattern) fromSyn) alts
  S.Abs xs a -> do
    a' <- fromSyn a
    pure $ foldr (T.Lam . T.Free) a' xs
  S.Let binds body -> do
    body'  <- fromSyn body
    binds' <- mapM (bimapM (pure . T.Free) fromSyn) binds
    pure $ T.Let binds' body'
  S.UnitLit -> pure T.Unit
  S.TupleLit es ->
    let con =
            T.Con
              (T.Free
                (TopLevel "Lam.Primitive" (Name ("Tuple" <> show (length es))))
              )
    in  foldl T.App con <$> mapM fromSyn es
  S.ListLit es          -> T.List <$> mapM fromSyn es
  S.StringLit prefix [] -> pure $ T.String prefix
  S.StringLit prefix comps ->
    let append = T.VarExp (T.Free "Lam.Primitive.appendString")
    in  foldl
            (\acc (c, s) -> T.App (T.App append acc) (T.App (T.App append c) s))
            (T.String prefix)
          <$> mapM (firstM fromSyn . second T.String) comps
  S.CharLit c -> pure $ T.Char c
  S.IntLit  i -> pure $ T.Int i
  S.BoolLit b -> pure $ T.Bool b
  S.Record r ->
    T.Record <$> mapM (secondM fromSyn . first (show . fromLocal)) r
  S.Project r f    -> T.Project <$> fromSyn r <*> pure (show (fromLocal f))
  S.FCall   n args -> T.FCall n <$> mapM fromSyn args

convertPattern :: Can.Pattern -> T.Pattern
convertPattern = \case
  S.VarPat v          -> S.VarPat (T.Free v)
  S.ConsPat c subpats -> S.ConsPat (T.Free c) (map convertPattern subpats)
  S.TuplePat subpats  -> S.TuplePat (map convertPattern subpats)
  S.ListPat  subpats  -> S.ListPat (map convertPattern subpats)
  S.WildPat           -> S.WildPat
  S.UnitPat           -> S.UnitPat
  S.IntPat    i       -> S.IntPat i
  S.CharPat   c       -> S.CharPat c
  S.BoolPat   b       -> S.BoolPat b
  S.StringPat s       -> S.StringPat s

convertScheme :: Can.Scheme -> T.TypeM T.Type
convertScheme (S.Forall vars ty) = do
  uMap <- mapM (\v -> (v, ) <$> T.newU v) vars
  ty'  <- convertType uMap ty
  pure $ foldr (T.Forall . snd) ty' uMap

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
    let name = fromString $ "Lam.Primitive.Tuple" <> show (length as)
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
  S.TyList -> pure $ T.TCon "Lam.Primitive.List" []
  S.TyRecord fields ->
    T.TRecord <$> mapM (secondM (convertType uVarCtx) . first toString) fields
  S.TyAlias _ _ ->
    T.throwError $ T.TodoError "convertType: type aliases not implemented"
  S.TyForall v t -> do
    u  <- T.newU v
    t' <- convertType ((v, u) : uVarCtx) t
    pure $ T.Forall u t'

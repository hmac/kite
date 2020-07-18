module Type.FromSyn where

-- Convert Syn to Type.Exp, ready for typechecking

import           Util

import           Data.Name                      ( Name(TopLevel)
                                                , RawName(Name)
                                                , fromLocal
                                                )
import           Type                           ( Exp )
import qualified Type                          as T

import qualified Canonical                     as Can
import qualified Syn                           as S

fromSyn :: Can.Exp -> T.TypeM Exp
fromSyn = \case
  S.LetA x ty e body ->
    T.Ann
      <$> (T.Let1 (T.Free x) <$> fromSyn e <*> fromSyn body)
      <*> convertScheme ty
  S.Var  n  -> pure $ T.VarExp (T.Free n)
  S.Con  n  -> pure $ T.Con (T.Free n)
  S.Hole n  -> pure $ T.Hole (show n)
  S.App a b -> T.App <$> fromSyn a <*> fromSyn b
  S.Case scrut alts ->
    T.Case
      <$> fromSyn scrut
      <*> mapM (bimapM (pure . convertPattern) fromSyn) alts
  S.MCase _  -> error "Type.fromSyn: MCase not implemented"
  S.Abs xs a -> do
    a' <- fromSyn a
    pure $ foldr (T.Lam . T.Free) a' xs
  S.Let binds body -> do
    body'  <- fromSyn body
    binds' <- mapM (secondM fromSyn) binds
    pure $ foldr (\(x, e) b -> T.Let1 (T.Free x) e b) body' binds'
  S.UnitLit -> pure $ T.Con (T.Free "Lam.Primitive.Unit")
  S.TupleLit es ->
    let con =
            T.Con
              (T.Free
                (TopLevel "Lam.Primitive" (Name ("Tuple" <> show (length es))))
              )
    in  foldr T.App con <$> mapM fromSyn es
  S.ListLit es ->
    foldr T.App (T.Con (T.Free "Lam.Primitive.List")) <$> mapM fromSyn es
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
  pure $ foldr T.Forall ty' (map snd uMap)

convertType :: [(Name, T.U)] -> Can.Type -> T.TypeM T.Type
convertType uVarCtx = \case
  S.TyBool  -> pure T.bool
  S.TyVar v -> case lookup v uVarCtx of
    Just u  -> pure $ T.UType u
    Nothing -> T.throwError $ T.UnknownVariable mempty (T.Free v)
  S.TyCon c   -> pure $ T.TCon c []
  S.TyApp a b -> do
    convertType uVarCtx a >>= \case
      T.TCon c args -> do
        b' <- convertType uVarCtx b
        pure $ T.TCon c $ args ++ [b']
      other ->
        T.throwError
          $  T.TodoError
          $  "convertType: cannot handle applications of non constructors: "
          <> show other
  S.TyList ->
    T.throwError $ T.TodoError "convertType: cannot convert bare List type"

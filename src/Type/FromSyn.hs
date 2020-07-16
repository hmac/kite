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

fromSyn :: Can.Exp -> Exp
fromSyn = \case
  S.Var  n  -> T.VarExp (T.Free n)
  S.Con  n  -> T.Con (T.Free n)
  S.Hole n  -> T.Hole (show n)
  S.App a b -> T.App (fromSyn a) (fromSyn b)
  S.Case scrut alts ->
    T.Case (fromSyn scrut) (map (bimap convertPattern fromSyn) alts)
  S.Abs xs a -> foldr (T.Lam . T.Free) (fromSyn a) xs
  S.Let binds body ->
    foldr (\(x, e) b -> T.Let1 (T.Free x) (fromSyn e) b) (fromSyn body) binds
  S.LetA x ty e body ->
    T.Ann (T.Let1 (T.Free x) (fromSyn e) (fromSyn body)) (convertScheme ty)
  S.UnitLit -> T.Con (T.Free "Lam.Primitive.Unit")
  S.TupleLit es ->
    let con =
            T.Con
              (T.Free
                (TopLevel "Lam.Primitive" (Name ("Tuple" <> show (length es))))
              )
    in  foldr (T.App . fromSyn) con es
  S.ListLit es ->
    foldr (T.App . fromSyn) (T.Con (T.Free "Lam.Primitive.List")) es
  S.StringLit prefix [] -> T.String prefix
  S.StringLit prefix comps ->
    let append = T.VarExp (T.Free "Lam.Primitive.appendString")
    in  foldl
          (\acc (c, s) -> T.App (T.App append acc) (T.App (T.App append c) s))
          (T.String prefix)
          (map (bimap fromSyn T.String) comps)
  S.CharLit c      -> T.Char c
  S.IntLit  i      -> T.Int i
  S.BoolLit b      -> T.Bool b
  S.Record  r      -> T.Record (map (bimap (show . fromLocal) fromSyn) r)
  S.Project r f    -> T.Project (fromSyn r) (show (fromLocal f))
  S.FCall   n args -> T.FCall n (map fromSyn args)

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

-- TODO: we may need to generate unique U variables here, but also keep hold of
-- the original names to provide useful errors.
convertScheme :: Can.Scheme -> T.Type
convertScheme = undefined

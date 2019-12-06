{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Desugar where

-- This module translates the surface syntax (Syn) to a smaller core syntax.
-- This is mainly to make it easier to infer and check types.
--
-- We perform the following translations:
-- - convert interpolated string to applications of `show` and `concat`
-- - convert case expressions to inline function definitions (TODO)
-- - convert lambda abstractions to let bindings (TODO)

import           Data.Bifunctor                 ( second
                                                , first
                                                )
import           GHC.Generics                   ( Generic )
import qualified Syntax                        as S
import           Syntax                         ( Name(..)
                                                , Syn
                                                , Pattern
                                                )

-- TODO: desugar Abs to Let
-- e.g. \x -> e ====> let f x = e in f
-- (requires patterns in Let bindings)
data Core = Var Name
          | Cons Name
          | Hole Name
          | Abs Name Core
          | App Core Core
          | Let [(Name, Core)] Core
          | Case Core [(S.Pattern, Core)]
          | Tuple [Core]
          | List [Core]
          | IntLit Int
          | FloatLit Float
          | StringLit String
          deriving (Eq, Show, Generic)

desugarModule :: S.Module S.Syn -> S.Module Core
desugarModule m = m { S.moduleDecls = map desugarDecl (S.moduleDecls m) }

desugarDecl :: S.Decl S.Syn -> S.Decl Core
desugarDecl (S.FunDecl       f) = S.FunDecl (desugarFun f)
desugarDecl (S.TypeclassInst i) = S.TypeclassInst (desugarInstance i)
desugarDecl (S.DataDecl      d) = S.DataDecl d
desugarDecl (S.TypeclassDecl t) = S.TypeclassDecl t
desugarDecl (S.Comment       c) = S.Comment c

desugarFun :: S.Fun S.Syn -> S.Fun Core
desugarFun f = f { S.funDefs = map desugarDef (S.funDefs f) }

desugarInstance :: S.Instance S.Syn -> S.Instance Core
desugarInstance i =
  i { S.instanceDefs = mapSnd (map desugarDef) (S.instanceDefs i) }

desugarDef :: S.Def S.Syn -> S.Def Core
desugarDef d = d { S.defExpr = desugarExpr (S.defExpr d) }

desugarExpr :: Syn -> Core
desugarExpr (S.Var  n         ) = Var n
desugarExpr (S.Cons n         ) = Cons n
desugarExpr (S.Hole n         ) = Hole n
desugarExpr (S.Abs  vars  e   ) = expandAbs vars (desugarExpr e)
desugarExpr (S.App  a     b   ) = App (desugarExpr a) (desugarExpr b)
desugarExpr (S.Let  binds e   ) = Let (mapSnd desugarExpr binds) (desugarExpr e)
desugarExpr (S.Case e     pats) = Case (desugarExpr e) (mapSnd desugarExpr pats)
desugarExpr (S.TupleLit es    ) = Tuple (map desugarExpr es)
desugarExpr (S.ListLit  es    ) = List (map desugarExpr es)
desugarExpr (S.StringLit prefix interps) =
  desugarString prefix (mapFst desugarExpr interps)
desugarExpr (S.IntLit   i) = IntLit i
desugarExpr (S.FloatLit f) = FloatLit f

-- Convert a multi-var lambda to nested single-var lambdas
-- \a b c -> e ==> \a -> \b -> \c -> e
expandAbs :: [Name] -> Core -> Core
expandAbs vs e = foldr Abs e vs

-- Convert a string interpolation into applications of concat and show
-- "x: #{a}, y: #{b}" ==> concat ["x: ", show a, ", y :", show b, ""]
-- TODO: this needs to be independent of whatever variables are in scope
-- ideally concat is part of Monoid/Semigroup and show is part of Show
desugarString :: String -> [(Core, String)] -> Core
desugarString prefix interps = App (Var "$prim_stringconcat")
                                   (List (StringLit prefix : go interps))
 where
  go []            = []
  go ((e, s) : is) = App (Var "$prim_show") e : StringLit s : go is

-- case scrut of
--   pat1 -> e1
--   pat2 -> e2
--
-- becomes:
--
-- let $fresh_name pat1 = e1
--     $fresh_name pat2 = e2
--  in $fresh_name scrut
--
-- NOTE: this doesn't work yet because lets don't support patterns
desugarCase :: Syn -> [(Pattern, Syn)] -> Core
desugarCase scrut alts = Let undefined undefined

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd = map . second

mapFst :: (a -> c) -> [(a, b)] -> [(c, b)]
mapFst = map . first

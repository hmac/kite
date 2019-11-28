{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Desugar where

import           Data.Bifunctor                 ( second
                                                , first
                                                )
import           GHC.Generics                   ( Generic )
import qualified Syntax                        as S
import           Syntax                         ( Name(..)
                                                , Syn
                                                )

data Core = Var Name
          | Cons Name
          | Hole Name
          | Abs Name Core
          | App Core Core
          | Let Name Core Core -- what about letrecs?
          | Case Core [(S.Pattern, Core)]
          | Tuple [Core]
          | List [Core]
          | IntLit Int
          | FloatLit Float
          | StringLit String
          deriving (Eq, Show, Generic)

desugarExpr :: Syn -> Core
desugarExpr (S.Var  n    ) = Var n
desugarExpr (S.Cons n    ) = Cons n
desugarExpr (S.Hole n    ) = Hole n
desugarExpr (S.Abs vars e) = expandAbs vars (desugarExpr e)
desugarExpr (S.App a    b) = App (desugarExpr a) (desugarExpr b)
desugarExpr (S.Let binds e) =
  expandLet (mapSnd desugarExpr binds) (desugarExpr e)
desugarExpr (S.Case e pats) = Case (desugarExpr e) (mapSnd desugarExpr pats)
desugarExpr (S.TupleLit es) = Tuple (map desugarExpr es)
desugarExpr (S.ListLit  es) = List (map desugarExpr es)
desugarExpr (S.StringLit prefix interps) =
  desugarString prefix (mapFst desugarExpr interps)
desugarExpr (S.IntLit   i) = IntLit i
desugarExpr (S.FloatLit f) = FloatLit f

-- Convert a multi-var lambd to nested single-var lambdas
-- \a b c -> e ==> \a -> \b -> \c -> e
expandAbs :: [Name] -> Core -> Core
expandAbs vs e = foldr Abs e vs

-- Convert a multi-bind let to nested lets
-- let a = b
--     c = d
--  in e
--
-- becomes
--
-- let a = b
--  in let c = d
--      in e
expandLet :: [(Name, Core)] -> Core -> Core
expandLet []            e = e
expandLet ((n, b) : bs) e = Let n b (expandLet bs e)

-- Convert a string interpolation into applications of concat and show
-- "x: #{a}, y: #{b}" ==> concat ["x: ", show a, ", y :", show b, ""]
-- TODO: this needs to be independent of whatever variables are in scope
-- ideally concat is part of Monoid/Semigroup and show is part of Show
desugarString :: String -> [(Core, String)] -> Core
desugarString prefix interps = App (Var "concat")
                                   (List (StringLit prefix : go interps))
 where
  go []            = []
  go ((e, s) : is) = App (Var "show") e : StringLit s : go is

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd = map . second

mapFst :: (a -> c) -> [(a, b)] -> [(c, b)]
mapFst = map . first

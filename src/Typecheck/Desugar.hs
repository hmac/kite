{-# LANGUAGE DeriveGeneric #-}
module Typecheck.Desugar where

-- This module translates the surface syntax (Syn) to a smaller core syntax used
-- for typechecking.
--
-- We perform the following translations:
-- - convert interpolated strings to applications of `show` and `concat`
-- - convert case expressions to inline function definitions
-- - convert lambda abstractions to let bindings

import           Util
import           GHC.Generics                   ( Generic )
import qualified Syntax                        as S
import           Syntax                         ( Name
                                                , Syn
                                                , Pattern
                                                )

data Core = Var Name
          | Cons Name
          | Hole Name
          | App Core Core
          | Let [(Name, [([S.Pattern], Core)])] Core
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
desugarExpr (S.Var  n     ) = Var n
desugarExpr (S.Cons n     ) = Cons n
desugarExpr (S.Hole n     ) = Hole n
desugarExpr (S.Abs vars  e) = desugarAbs vars (desugarExpr e)
desugarExpr (S.App a     b) = App (desugarExpr a) (desugarExpr b)
desugarExpr (S.Let binds e) = Let (map desugarLetBind binds) (desugarExpr e)
  where desugarLetBind (n, a) = (n, [([], desugarExpr a)])
desugarExpr (S.Case e pats) = desugarCase e pats
desugarExpr (S.TupleLit es) = Tuple (map desugarExpr es)
desugarExpr (S.ListLit  es) = List (map desugarExpr es)
desugarExpr (S.StringLit prefix interps) =
  desugarString prefix (mapFst desugarExpr interps)
desugarExpr (S.IntLit   i) = IntLit i
desugarExpr (S.FloatLit f) = FloatLit f

-- Convert lambda abstractions to inline let bindings
-- \x -> e
--
-- becomes
--
-- let f x = e in f
desugarAbs :: [Name] -> Core -> Core
desugarAbs vars e =
  let freshName = "$f"
  in  Let [(freshName, [(map S.VarPat vars, e)])] (Var freshName)

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
desugarCase :: Syn -> [(Pattern, Syn)] -> Core
desugarCase scrut alts =
  -- TODO: generate an actual fresh, unique name
  let freshName = "$f"
      pats      = map (\(pat, a) -> ([pat], desugarExpr a)) alts
  in  Let [(freshName, pats)] (App (Var freshName) (desugarExpr scrut))

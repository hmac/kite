module Typecheck.Desugar where

-- This module translates the surface syntax (Syn) to a smaller core syntax used
-- for typechecking.
--
-- We perform the following translations:
-- - convert interpolated strings to applications of `show` and `concat`
-- - convert case expressions to inline function definitions
-- - convert lambda abstractions to let bindings

import           Util
import           Data.Name
import qualified Canonical                     as Can
import qualified Syntax                        as S
import           Syntax                         ( Name )

data Core = Var Can.Name
          | Cons Can.Name
          | Hole Can.Name
          | App Core Core
          | Let [(Can.Name, [([Can.Pattern], Core)])] Core
          | Tuple [Core]
          | List [Core]
          | IntLit Int
          | StringLit String
          deriving (Eq, Show)

desugarModule :: Can.Module Can.Exp -> Can.Module Core
desugarModule m = m { S.moduleDecls = map desugarDecl (S.moduleDecls m) }

desugarDecl :: Can.Decl Can.Exp -> Can.Decl Core
desugarDecl (S.FunDecl       f) = S.FunDecl (desugarFun f)
desugarDecl (S.TypeclassInst i) = S.TypeclassInst (desugarInstance i)
desugarDecl (S.DataDecl      d) = S.DataDecl d
desugarDecl (S.TypeclassDecl t) = S.TypeclassDecl t
desugarDecl (S.Comment       c) = S.Comment c

desugarFun :: Can.Fun Can.Exp -> Can.Fun Core
desugarFun f = f { S.funDefs = map desugarDef (S.funDefs f) }

desugarInstance :: Can.Instance Can.Exp -> Can.Instance Core
desugarInstance i =
  i { S.instanceDefs = mapSnd (map desugarDef) (S.instanceDefs i) }

desugarDef :: Can.Def Can.Exp -> Can.Def Core
desugarDef d = d { S.defExpr = desugarExpr (S.defExpr d) }

desugarExpr :: Can.Exp -> Core
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
desugarExpr (S.IntLit i) = IntLit i

-- Convert lambda abstractions to inline let bindings
-- \x -> e
--
-- becomes
--
-- let f x = e in f
desugarAbs :: [Can.Name] -> Core -> Core
desugarAbs vars e =
  let freshName = Can.Local "$f"
  in  Let [(freshName, [(map S.VarPat vars, e)])] (Var freshName)

-- Convert a string interpolation into applications of appendString and show
-- "x: #{a}, y: #{b}" ==> "x: " <> show a <> ", y :" <> show b <> ""
desugarString :: String -> [(Core, String)] -> Core
desugarString prefix interps = foldr (\x acc -> App (App appendString x) acc)
                                     (StringLit "")
                                     (StringLit prefix : go interps)
 where
  appendString = Var (Can.TopLevel modPrim "appendString")
  go [] = []
  go ((e, s) : is) =
    App (Var (Can.TopLevel modPrim "show")) e : StringLit s : go is

-- TODO: deduplicate with ELC.Primitive
modPrim :: ModuleName
modPrim = ModuleName ["Lam", "Primitive"]

-- case scrut of
--   pat1 -> e1
--   pat2 -> e2
--
-- becomes:
--
-- let $fresh_name pat1 = e1
--     $fresh_name pat2 = e2
--  in $fresh_name scrut
desugarCase :: Can.Exp -> [(Can.Pattern, Can.Exp)] -> Core
desugarCase scrut alts =
  -- TODO: generate an actual fresh, unique name
  let freshName = Can.Local "$f"
      pats      = map (\(pat, a) -> ([pat], desugarExpr a)) alts
  in  Let [(freshName, pats)] (App (Var freshName) (desugarExpr scrut))

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chez.Compile
  ( Env(..)
  , compileModule
  )
where

import           Prelude                 hiding ( null )
import           Data.Name                      ( Name(..)
                                                , RawName(..)
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Chez                           ( SExpr(..)
                                                , Lit(..)
                                                , Def(..)
                                                , null
                                                )
import qualified Syn.Typed                     as T
import           Data.List.Extra                ( concatUnzip )

-- Compile Kite to Chez Scheme

-- Data types
-- ----------
--
-- Data types are compiled to Scheme record types
--
-- Product types are records with a field for each constructor argument, numbered _0 ... _N.
--
-- Sum types are records with numbered fields equal to the maximum number of args of their
-- constructor. They have an additional field _tag which indicates which constructor the object
-- belongs to. This is 0 for the first constructor (in source-code order), 1 for the second etc.
--
-- For each constructor we define a function which constructs a value with the correct tag.
-- This requires that constructors are unambiguous, so you can't have two constructors with the same
-- name in the same module. We can relax this if we compile constructor call sites directly into
-- applications of (make-<type> ...) instead.
--
-- Records are compiled to hashtables. Since they don't have constructors, we don't need to give
-- them specific constructors in scheme (I don't think).

-- Examples:
--
-- type Pair a b = Pair a b
-- (define-record-type Pair (fields _0 _1))
--
-- type List a = Nil | Cons a (List a)
-- (define-record-type List (fields _tag _0 _1))
--
-- type Functor f = Functor { map : forall a b. (a -> b) -> f a -> f b }
-- (define record-type Functor (fields _0))

-- Case expressions
-- ----------------
--
-- A single-scrutinee case expression is compiled to a cond expression which tests the _tag field of
-- the type. Each branch of the cond starts with a let expression to bind the captured fields of the
-- constructor.
--
-- Example:
--
-- case (l : List Int) of
--   Nil       -> 0
--   Cons x xs -> x
--
-- (cond ((eq? 0 (List-_tag l)) 0)
--       ((eq? 1 (List-_tag l)) (let ((x (List-_0 l))
--                                    (xs (List-_1 l)))
--                                    x)))
--
-- Multi-scrutinee case expressions (i.e. mcase) are more difficult to compile efficiently, because
-- we want to minimise the number of tests we do. The pattern match compiler from ELC shows how to
-- do this, so we should be able to port that logic here.

newtype Env = Env { envDefs :: [Def] }
  deriving (Eq, Show, Semigroup, Monoid)

compileModule :: Env -> T.Module -> Env
compileModule Env { envDefs = defs } m =
  Env { envDefs = defs ++ concatMap compileDecl (T.moduleDecls m) }

compileDecl :: T.Decl -> [Def]
compileDecl = \case
  T.FunDecl fun ->
    [Def (name2Text (T.funName fun)) (compileExpr (T.funExpr fun))]
  T.DataDecl d -> compileData d

compileData :: T.Data -> [Def]
compileData dat =
      -- The maximum number of fields for a constructor of this type
  let
    maxFields        = maximum $ map (length . T.conArgs) (T.dataCons dat)
    fields           = "_tag" : map (pack . ('_' :) . show) [0 .. maxFields - 1]
    typeName         = name2Text $ T.dataName dat
    recordDefinition = DefRecord typeName fields
    -- Each data type constructor is compiled to a function which constructs an object of that type with
    -- the correct _tag.
    mkConstructor :: Int -> T.DataCon -> Def
    mkConstructor tag con =
      let
        maxArgs     = length $ T.conArgs con
        parameters  = map (pack . ('_' :) . show) [0 .. maxArgs - 1]
        fieldValues = Lit (Int tag) : map Var parameters <> take
          (maxFields - maxArgs)
          (repeat null)
      in
        Def (name2Text (T.conName con)) $ Abs parameters $ App
          (Var ("make-" <> typeName))
          fieldValues
  in
    recordDefinition : zipWith mkConstructor [0 ..] (T.dataCons dat)

compileExpr :: T.Exp -> SExpr
compileExpr = \case
  T.IntLitT  n           -> Lit (Int n)
  T.BoolLitT b           -> Lit (Bool b)
  T.CharLitT c           -> Lit (Char c)
  T.UnitLitT             -> Lit Unit
  T.StringLitT s         -> Lit (String s)
  T.TupleLitT elems _    -> List (map compileExpr elems)
  T.ListLitT  elems _    -> Vec (map compileExpr elems)
  T.AnnT      e     _    -> compileExpr e
  T.VarT      v     _    -> Var $ name2Text v
  T.ConT c        _    _ -> Var $ name2Text c
  T.AbsT vars     body _ -> Abs (map (name2Text . fst) vars) (compileExpr body)
  T.AppT f        arg  _ -> App (compileExpr f) [compileExpr arg]
  T.LetT bindings body _ -> Let
    (map (\(n, e, _) -> (name2Text n, compileExpr e)) bindings)
    (compileExpr body)
  -- TODO: we duplicate the scrutinee here, which is inefficient if it's not a variable.
  -- We should bind it in a let first (generating a fresh name)
  T.CaseT scrut alts _ ->
    let scrutExpr = compileExpr scrut
    -- We need to convert each constructor pattern into a test on the tag, but to do that we need to
    -- know how to map constructors to tags.
        altExprs  = map (uncurry compileAlt) alts
        -- Each case alt is compiled to a test ((eq? <tag> (<type>-_tag <scrut>)) <rhs>)
        compileAlt :: T.Pattern -> T.Exp -> SExpr
        compileAlt pat rhs =
            let (tests, bindings) = compilePat pat scrutExpr
            in  List [App (Var "and") tests, Let bindings (compileExpr rhs)]
    in  App (Var "cond") altExprs
  -- An mcase takes N arguments and matches each against a pattern simultaneously.
  -- We compile it to a lambda that takes N arguments and then tests them in a cond, similar to
  -- case.
  T.MCaseT [] _ ->
    error $ "Chez.Compile.compileExpr: Cannot compile empty mcase"
  T.MCaseT alts@((ps, _) : _) _ ->
    let argNum = length ps
        vars   = map (\n -> "fixme" <> pack (show n)) [0 .. argNum - 1]
        compileAlt :: [T.Pattern] -> T.Exp -> SExpr
        compileAlt pats rhs =
            let (tests, bindings) = compileMCaseBranch (map Var vars) pats
            in  List [App (Var "and") tests, Let bindings (compileExpr rhs)]
        altExprs = map (uncurry compileAlt) alts
    in  Abs vars $ App (Var "cond") altExprs
  -- TODO:
  -- - String interpolation
  -- - Records
  -- - Foreign calls
  -- - Holes
  e -> error $ "Cannot compile " <> show e <> "yet"

compileMCaseBranch :: [SExpr] -> [T.Pattern] -> ([SExpr], [(Text, SExpr)])
compileMCaseBranch _ [] = ([], [])
compileMCaseBranch (v : vars) (p : pats) =
  let (tests     , bindings     ) = compilePat p v
      (innerTests, innerBindings) = compileMCaseBranch vars pats
  in  (tests <> innerTests, bindings <> innerBindings)
compileMCaseBranch [] _ =
  error "Chez.Compile.compileMCaseBranch: empty set of variables"

-- Given a pattern p, and a scrutinee s, 'compilePat p s' compiles the pattern, returning a list of
-- tests to determine if the pattern matches and a list of bindings to construct if it matches.
-- TODO: the scrutinee should be a variable, to avoid duplicating it.
compilePat :: T.Pattern -> SExpr -> ([SExpr], [(Text, SExpr)])
compilePat pattern scrut = case pattern of
  T.VarPat x       -> ([], [(name2Text x, scrut)])
  T.WildPat        -> ([], [])
  T.UnitPat        -> ([], [])
  T.IntPat    n    -> ([App (Var "eq?") [scrut, Lit (Int n)]], [])
  T.CharPat   c    -> ([App (Var "eq?") [scrut, Lit (Char c)]], [])
  T.StringPat s    -> ([App (Var "eq?") [scrut, Lit (String s)]], [])
  T.BoolPat   b    -> ([App (Var "eq?") [scrut, Lit (Bool b)]], [])
  T.TuplePat  pats -> concatUnzip $ zipWith
    (\p i -> compilePat p (App (Var "vector-ref") [scrut, Lit (Int i)]))
    pats
    [0 :: Int ..]
  T.ListPat pats ->
    let (tests, bindings) = concatUnzip $ zipWith
          (\p i -> compilePat p (App (Var "list-ref") [scrut, Lit (Int i)]))
          pats
          [0 :: Int ..]
        lengthTest = App
          (Var "eq?")
          [App (Var "length") [scrut], Lit (Int (length pats))]
    in  (lengthTest : tests, bindings)
  -- TODO: we could use record-accessor to index into the fields of the constructor
  -- https://scheme.com/tspl4/records.html#./records:h1
  T.ConsPat c Nothing _ ->
    error $ "Chez.Compile.compilePat: no metadata for constructor " <> show c
  T.ConsPat _c (Just meta) pats ->
    let tag      = T.conMetaTag meta
        ty       = name2Text $ T.conMetaTypeName meta
        tagField = ty <> "-" <> "_tag"
        fieldAtIndex i = ty <> "-_" <> pack (show i)
        ctorTest = App (Var "eq?") [App (Var tagField) [scrut], Lit (Int tag)]
        (tests, bindings) = concatUnzip $ zipWith
          (\p i -> compilePat p (App (Var (fieldAtIndex i)) [scrut]))
          pats
          [0 :: Int ..]
    in  (ctorTest : tests, bindings)

name2Text :: Name -> Text
name2Text (Local (Name n)              ) = pack n
name2Text (TopLevel moduleName (Name n)) = pack $ show moduleName ++ "." ++ n

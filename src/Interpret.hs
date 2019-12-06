{-# LANGUAGE OverloadedStrings #-}
module Interpret where

import           Data.Maybe                     ( fromMaybe )
import           Syntax                         ( Name(..)
                                                , Module(..)
                                                , Pattern(..)
                                                , Def(..)
                                                , Decl(..)
                                                , Fun(..)
                                                )
import           Desugar
import           Prelude                 hiding ( lookup )
import qualified Prelude                        ( lookup )

evalExpr :: Module Core -> Core -> Core
evalExpr m e = eval (buildContext m) e

-- Construct a context from all top level declarations in a module
-- In future: also follow imports
buildContext :: Module Core -> Context
buildContext m = foldMap decltoContext (moduleDecls m)
 where
  decltoContext (FunDecl f) = funToContext f
  decltoContext _           = mempty
  funToContext f = singleton (funName f) (mkLet (funName f) (funDefs f))
  mkLet (Name n) defs =
    let name = Name ("$let_" ++ n) -- this may not be necessary
    in  Let [(name, map (\d -> (defArgs d, defExpr d)) defs)] (Var name)

-- A context is a map from names to expressions.
-- We use a function type so we can add arbitrary lookups to the context.
-- This is useful because Core doesn't have a constructor for lambda
-- abstraction, so all reduction has to use lets.
newtype Context = Context (Name -> Maybe Core)

singleton :: Name -> Core -> Context
singleton n e = extend n e mempty

lookup :: Name -> Context -> Core
lookup n (Context c) = fromMaybe (error ("unknown variable: " <> show n)) (c n)

-- The first context takes precedence
merge :: Context -> Context -> Context
merge (Context c) (Context c') = Context $ \m -> case c m of
  Nothing -> c' m
  r       -> r

concat :: [Context] -> Context
concat = foldl1 merge

-- Extend a context with a new binding
extend :: Name -> Core -> Context -> Context
extend n e (Context c) = Context $ \m -> if m == n then Just e else c m

instance Semigroup Context where
  (<>) = merge

instance Monoid Context where
  mempty = Context (const Nothing)

-- TODO: this would be a lot easier if we first converted to an AST with lambda
-- abstraction and support for primitive functions.
-- Maybe we should convert from the surface syntax instead of the desugared?

eval :: Context -> Core -> Core
eval _ (IntLit    i    ) = IntLit i
eval _ (StringLit s    ) = StringLit s
eval _ (FloatLit  f    ) = FloatLit f
eval c (List      elems) = List $ map (eval c) elems
eval c (Tuple     elems) = Tuple $ map (eval c) elems
eval _ (Hole      n    ) = error $ "encountered hole: " <> show n
eval c (Cons      n    ) = lookup n c
eval c (Var       n    ) = case Prelude.lookup n primitives of
  Just _  -> Var n
  Nothing -> eval c (lookup n c)
eval c (App (Var n) b) = case Prelude.lookup n primitives of
  Just f  -> f (eval c b)
  Nothing -> eval c $ App (eval c (Var n)) (eval c b)
eval c (App a     b) = eval c $ App (eval c a) (eval c b)
eval c (Let binds e) = evalLet c binds e

-- Evaluate an expression in the context of a let binding
evalLet :: Context -> [(Name, [([Pattern], Core)])] -> Core -> Core
evalLet ctx binds e =
  let ctx' = merge (foldMap (bindToContext ctx) binds) ctx in eval ctx' e
 where
  bindToContext :: Context -> (Name, [([Pattern], Core)]) -> Context
  bindToContext c (n, alts) =
    Context $ \m -> if n == m then Just (select c [e] alts) else Nothing

-- Match a list of expressions against a list of patterns, stopping at the
-- first match. Return the expression corresponding to that pattern, after
-- evaluating it using the updated context.
select :: Context -> [Core] -> [([Pattern], Core)] -> Core
select _ _  []                   = error "no matching pattern"
select c es ((pats, res) : rest) = case matchAll c es pats of
  (False, _ ) -> select c es rest
  (True , c') -> eval (merge c' c) res

-- Match a list of expressions against a list of patterns.
-- If it matches, returns a context of variables bound by the patterns.
-- Precondition: the lists are the same length
-- (should be guaranteed by typechecking)
matchAll :: Context -> [Core] -> [Pattern] -> (Bool, Context)
matchAll c _ [] = (True, c)
matchAll c es pats =
  let matches = zipWith (match c) es pats
  in  if all fst matches then (True, foldMap snd matches) else (False, mempty)

-- Match an expression against a pattern
-- If it matches, returns a context of variables bound by the pattern
match :: Context -> Core -> Pattern -> (Bool, Context)
match _ e          (VarPat v) = (True, singleton v e)
match _ _          WildPat    = (True, mempty)
match _ (IntLit i) (IntPat j) = (i == j, mempty)
match c (Tuple as) (TuplePat bs) | length as == length bs =
  let submatches = zipWith (match c) as bs
  in  if all fst submatches
        then (True, foldMap snd submatches)
        else (False, mempty)
match c (List as) (ListPat bs) | length as == length bs =
  let submatches = zipWith (match c) as bs
  in  if all fst submatches
        then (True, foldMap snd submatches)
        else (False, mempty)
match c e (ConsPat m pats) =
  let (Cons n : args) = unfoldApp (eval c e)
      submatches      = zipWith (match c) args pats
  in  if n == m && length args == length pats && all fst submatches
        then (True, foldMap snd submatches)
        else (False, mempty)
match _ _ _ = (False, mempty)

-- Assumes that an application has a Cons at its head, and unfolds it to a list
-- of arguments with the Cons at the front.
-- F a b (c d) ==> [F, a, b, (c d)]
unfoldApp :: Core -> [Core]
unfoldApp expr = go [] expr
 where
  go cs (App a b) = go (b : cs) a
  go cs _         = cs

-- Primitive operations

primitives :: [(Name, Core -> Core)]
primitives = [("$prim_stringconcat", primStringConcat)]

primStringConcat :: Core -> Core
primStringConcat (List strs) = StringLit $ concatMap (\(StringLit s) -> s) strs

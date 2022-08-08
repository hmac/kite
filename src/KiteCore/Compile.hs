{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module KiteCore.Compile where

import           AST                            ( fv
                                                , fvPat
                                                )
import           Control.Applicative            ( liftA2 )
import           Control.Lens                   ( _2
                                                , set
                                                , transformMOf
                                                , view
                                                )
import           Control.Monad.Writer.Strict    ( MonadTrans(lift)
                                                , WriterT(runWriterT)
                                                , tell
                                                )
import           Data.Data.Lens                 ( uniplate )
import           Data.Generics.Product
import           Data.Generics.Sum
import qualified Data.List
import           Data.List                      ( (\\) )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map.Strict               as Map
import           Data.Name                      ( Name
                                                , prim
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Tuple.Extra               ( fst3 )
import qualified KiteCore                      as Core
                                                ( Exp(..) )
import           Syn.Typed                      ( Exp
                                                , ExprT(..)
                                                , Pat(..)
                                                , Pattern
                                                , Type
                                                , typeOf
                                                )
import           Type.Type                      ( Type(TOther)
                                                , Type'(Fn, IFn)
                                                )
import           Util                           ( debug
                                                , first
                                                )

-- Transformations (in order of application):
--  1. Drop type annotations
--  2. Desguar string interpolation
--     e.g. "a #{b} #{c} d" ==> let elems = ["a ", b, " ", c, " d"] in concatString elems
--  3. Flatten nested applications (introduce lets if necessary)
--     e.g. (f g) x ==> f g x
--          f (g x) y ==> f (let h = g x in h) y
--  4. Lift lets out of applications
--     e.g. f (let h = g x in h) y ==> let h = g x in f h y
--  6. Merge adjacent lets
--     e.g. let x = 1 in let y = 2 in z ==> let x = 1; y = 2 in z
--  5. Convert IAbsT to MCase
--     e.g. f (x => y) z ==> f (x -> y) z
--  4. Merge adjacent mcase
--     e.g. (x -> (y -> z)) ==> (x y -> z)
--  7. Beta-reduction
--     e.g. (x -> y) z ==> y
--  8. Lift mcase to top-level pattern function
--     e.g. f (True -> y, False -> z) w ==> g(y, z, True) = y; g(y, z, False) = z
--                                          f g w
--  9. Convert top-level pattern function to case
--     e.g. f(y, 1, True) = y; f(y, z, x) = z ==> f(y, z, x) = case x of { True -> case z of { 1 -> y; _ -> z }; False -> z }

-- | Strip all type annotations from the expression.
-- The Monad constraint is required by 'param', but can be instantiated to
-- anything, e.g. 'Identity'.
removeTypeAnnotations :: Monad m => ExprT Name Type -> m Exp
removeTypeAnnotations = param @0 (const (pure ()))


-- Postponed:
-- 2. Desguar records to function calls
--     e.g. { a = 1 } ==> let fields = [("a", 1)] in makeRecord fields
--         r.a       ==> getField "a" r
-- For this to work, makeRecord and getField must be "unsafe", i.e. they can't
-- be expressed as typed Kite code. We don't have a nice mechanism for embedding
-- untyped Kite code in KiteCore at the moment, so we're punting this problem.

-- | Desugar a string interpolation into applications of the primitive function
-- appendString.
-- "a #{b} #{c} d" ==> appendString "a " (appendString b (appendString " " (appendString c (appendString " d"))))
-- TODO: This is more concise if we have a concatString primitive: let elems = ["a ", b, " ", c, " d"] in concatString elems
desugarStringInterpolation :: String -> NonEmpty (Exp, String) -> Exp
desugarStringInterpolation prefix components =
  let elems = foldMap (\(e, s) -> [e, StringLitT () s]) components
  in  foldl (AppT () . AppT () (VarT () (prim "appendString")))
            (StringLitT () prefix)
            elems

-- | Flatten an application, ensuring that it is in spine form.
-- This deals with two cases:
-- 1. (f x) y ==> f x y
-- 2. (f (g x)) y ==> f (let h = g x in h) y
-- The second case is later transformed by let lifting into let h = g x in f h y
flattenApplication :: forall m . Monad m => m Name -> Exp -> Exp -> m Exp
flattenApplication genName compoundHead lastArg = do
  -- First, unfold the head to get the true head and all other arguments in this application.
  let (trueHead, args) = unfoldApp compoundHead
  -- This has dealt with case 1. For case 2 we pass over the arguments, wrapping
  -- any applications in a let binding.
  args' <- mapM wrapLet (args <> [lastArg])
  -- Now we fold the application back up
  pure $ foldl (AppT ()) trueHead args'
 where
  unfoldApp :: Exp -> (Exp, [Exp])
  unfoldApp expr = let (f, revArgs) = go expr in (f, reverse revArgs)
   where
    go (AppT _ f x) = let (f', xs) = go f in (f', x : xs)
    go f            = (f, [])
  wrapLet :: Exp -> m Exp
  wrapLet e@AppT{} = do
    n <- genName
    pure $ LetT () [(n, e, Nothing)] (VarT () n)
  wrapLet e = pure e

-- | Lift a let out of an application.
-- KiteCore applications cannot contain let or case expressions.
-- When we encounter a let inside an application we have to lift the let above
-- the application. When doing this we must be careful to not to capture any
-- identically-name variables in the surrounding application. We avoid this by
-- renaming the let if it clashes with any variables that we lift it above.
-- For example, the expression
--  f x (let x = 1 in x + 1)
-- becomes
--  let z = 1 in f x (z + 1)
-- We assume that the caller has a reference to the outer expression and the
-- inner let.
-- The arguments are:
-- - a name generator
-- - the set of any free variables between the let and the parent application
-- - the original name bound by the let
-- - the bound value
-- - the body of the let
-- - the "let hole", which is a hole centered on the let. We use this to replace
--   the let in the parent application.
liftLet
  :: Monad m
  => m Name
  -> Set Name
  -> Name
  -> Exp
  -> Exp
  -> Maybe Type
  -> (Exp -> Exp)
  -> m Exp
liftLet genName freeVarsAboveLet letName letValue letBody letType letHole = do
    -- Check if we need to generate a new name for the let
    -- If so, rename any references to the old name in the let body
  (letBody', name) <- if Set.member letName freeVarsAboveLet
    then do
      name <- genName
      pure (renameVar letName name letBody, name)
    else pure (letBody, letName)

  -- Replace the let with its body
  let outerExp = letHole letBody'
  -- Wrap the outer expression with the let
  pure $ LetT (typeOf outerExp) [(name, letValue, letType)] outerExp

-- | Rename all instances of the given free variable.
renameVar :: Name -> Name -> Exp -> Exp
renameVar old = set $ traverseUntilNameShadow old . _Ctor @"VarT" . _2

-- | A traversal for 'Exp' that stops when it reaches a binding of the given
-- name. This is useful if you want to rename all instances of this name without
-- affecting identically-named bindings.
-- Note this is not quite a Traversal because it requires 'm' to be a Monad, not
-- just an Applicative. I don't think this should be necessary but it's the only
-- way I've worked it out so far.
traverseUntilNameShadow
  :: forall m . Monad m => Name -> (Exp -> m Exp) -> (Exp -> m Exp)
traverseUntilNameShadow name f = go
 where
  go :: Exp -> m Exp
  go = \case
    LetT t bindings body | any ((name ==) . fst3) bindings ->
        -- This let shadows our name, so don't recurse any further.
        -- However we still want to apply f to any values bound by the let _before_
        -- the shadowing binding.
      let (bindingsBefore, bindingsAfter) =
            Data.List.break ((name ==) . fst3) bindings
          bindingsBefore' =
            traverse (\(n, v, b) -> (n, , b) <$> go v) bindingsBefore
          bindingsAfter' = pure bindingsAfter
          bindings'      = liftA2 (<>) bindingsBefore' bindingsAfter'
      in  LetT t <$> bindings' <*> pure body
    expr@(IAbsT _ pat _ _) | any ((== name) . fst) (fvPat pat) ->
        -- This abstraction shadows our name, so don't recurse further.
      pure expr
    expr -> uniplate go expr >>= f

-- | Construct an error for a hole.
newHoleError :: Type -> Name -> Core.Exp Name
newHoleError = undefined

-- | Lift all lambdas in an expression.
-- This returns the resulting expression and a list of new top-level functions
-- that have been generated by lifting.
liftAllLambdas
  :: forall m
   . Monad m
  => m Name
  -> Exp
  -> m (Exp, [(Name, NonEmpty ([Pattern], Exp))])
liftAllLambdas genName = runWriterT . transformMOf uniplate f
 where
  f :: Exp -> WriterT [(Name, NonEmpty ([Pattern], Exp))] m Exp
  f (IAbsT t pat _ e) = do
    name <- lift genName
    let (pats, sub) = liftLambda name t [pat] e
    tell [(name, NE.singleton (pats, e))]
    pure sub
  f (MCaseT t branches) = do
    name <- lift genName
    let (newBranches, sub) = liftMCase name t branches
    tell [(name, newBranches)]
    pure sub
  f e = pure e

-- | @liftLambda name ty pat exp@ lifts a lambda with type @ty@ that binds
-- patterns @pats@ with body @exp@ to a separate function called @name@. It
-- returns a list of patterns of the new function, and the expression that
-- should be substituted for the original lambda expression.
-- The new function body should be the same as the old one.
-- For example, given this code        @let f = foo in (\y -> f y)@
-- we get a new function:              @new_func_1(f, y) = f y@
-- and the original expression becomes @let f = foo in new_func_1(f)@
-- Patterns are preserved in the output, for example:
--         @let f = foo in (\(x, y) -> f x y)@
-- becomes
--         @new_func_2(f, (x, y)) = f x y@
--         @let f = foo in new_func_2(f)@
liftLambda :: Name -> Type -> [Pattern] -> Exp -> ([Pattern], Exp)
liftLambda name ty pats body =
  let ((newPats, _) :| _, substitute) =
        liftMCase name ty $ NE.singleton (pats, body)
  in  (newPats, substitute)

-- | Same as liftLambda but for mcases. Returns the new set of patterns of the
-- lifted mcase, which is like the old patterns but prefixed with variable pats
-- for each free variable in the body of the original.
-- e.g. given @f (x -> y) z@ we get a new function @g(y, x) = x@ and a new
-- expression @f (g y) z@.
-- Here's a larger example:
-- @f (True 1 -> y; False n -> z) x@
-- @g = y z True 1 -> y; y z False n -> z@
-- @f (g y z) x@
liftMCase
  :: Name
  -> Type
  -> NonEmpty ([Pattern], Exp)
  -> (NonEmpty ([Pattern], Exp), Exp)
liftMCase name ty branches =
  -- The parameters of the new function are the free variables in the body of
  -- the mcase followed by the patterns of the mcase.
  -- We have to bind any free variable in any branch, since we don't know which
  -- branch we will take.
  let
    patternTypes :: [Type]
    patternTypes = map (view (typed @Type)) $ fst $ NE.head branches
    -- The free variables of an mcase are any variables in a branch rhs that
    -- are not bound in that branch's patterns.
    freeVars :: [(Name, Type)]
    freeVars = foldMap
      (\(pats, rhs) -> Map.toList (fv rhs) \\ foldMap fvPat pats)
      branches
    freeVarPatterns :: [Pattern]
    freeVarPatterns    = map (uncurry (flip VarPat)) freeVars
    -- Replace any IFn types in the mcase type with Fn types.
    -- This doesn't affect the compiled code but it makes testing easier and
    -- better matches the spirit of things (since implicits are all explicit
    -- at this point).
    functionResultType = foldl (\t _ -> getResultType t) ty patternTypes
     where
      getResultType = \case
        TOther (IFn _ t) -> t
        TOther (Fn  _ t) -> t
        t                -> t
    newFunctionType = foldr (\a b -> TOther (Fn a b))
                            functionResultType
                            (map snd freeVars <> patternTypes)
    newBranches = fmap (first (freeVarPatterns <>)) branches
    application = foldl f (VarT newFunctionType name) freeVars
     where
      f e (n, t) = AppT (funcTypeRange (typeOf e)) e (VarT t n)
      -- Given a function type `A -> B`, return `B`.
      -- If the type is not a function type, something has gone wrong in the
      -- typechecker so we crash.
      funcTypeRange :: Type -> Type
      funcTypeRange (TOther (Fn _ b)) = b
      funcTypeRange t = error $ "Expected a function type, got " <> debug t
  in
    (newBranches, application)

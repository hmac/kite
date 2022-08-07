{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module KiteCore.Compile where

import AST (fv, fvPat)
import Control.Applicative (liftA2)
import Control.Lens (set, transformMOf, _2)
import Control.Monad.Writer.Strict (MonadTrans (lift), WriterT (runWriterT), tell)
import Data.Data.Lens (uniplate)
import Data.Generics.Sum
import Data.List ((\\))
import qualified Data.List
import Data.List.Extra (mconcatMap)
import qualified Data.Map.Strict as Map
import Data.Name (Name)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (fst3)
import qualified KiteCore as Core (Exp (..))
import Syn.Typed (Exp, ExprT (..), Pat (..), Pattern, Type, typeOf)
import Type.Type (Type (TOther), Type' (Fn, IFn))
import Util (debug)

-- TODO: Consider stripping all types before this phase, to simplify the
-- transformations.

-- | Compile 'Syn.Typed.Exp' to 'KiteCore.Exp'.
-- compileExpToCore :: Exp -> Core.Exp Name
-- compileExpToCore = \case
--   VarT _ n -> Core.Atom (Core.Var n)
--   AnnT _ e _ -> compileExpToCore e
--   ConT _ _ meta -> Core.Atom (Core.Ctor (conMetaTag meta) [])
--   HoleT t n -> newHoleError t n
--   IAbsT _t _pat _ _e2 -> error "Unexpected IAbsT - this should have been lifted"
--   AppT t e1 e2 -> _
--   IAppT t e1 e2 -> _
--   LetT t e1 e2 -> _
--   CaseT t e alts -> _
--   MCaseT t alts -> error "Unexpected MCaseT - this should have been lifted"
--   UnitLitT t -> Core.Atom $ Core.Ctor 0 []
--   TupleLitT t elems ->
--     -- TODO: lift elems to let bindings if they're not atoms
--     Core.Atom $ Core.Ctor 0 $ map compileExpToCore elems
--   ListLitT t [] -> Core.Atom $ Core.Ctor 0 []
--   ListLitT t elems -> Core.Atom $ Core.Ctor 1 $ map compileExpToCore elems
--   StringInterpT t s comps -> _
--   StringLitT t s -> _
--   CharLitT t c -> _
--   IntLitT t i -> _
--   BoolLitT t b -> _
--   RecordT t fields -> _
--   ProjectT t r field -> _
--   FCallT t f args -> _
--   ImplicitT t i -> _

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
liftLet :: Monad m => m Name -> Set Name -> Name -> Exp -> Exp -> Maybe Type -> (Exp -> Exp) -> m Exp
liftLet genName freeVarsAboveLet letName letValue letBody letType letHole =
  do
    -- Check if we need to generate a new name for the let
    -- If so, rename any references to the old name in the let body
    (letBody', name) <-
      if Set.member letName freeVarsAboveLet
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
traverseUntilNameShadow :: forall m. Monad m => Name -> (Exp -> m Exp) -> (Exp -> m Exp)
traverseUntilNameShadow name f = go
  where
    go :: Exp -> m Exp
    go = \case
      LetT t bindings body
        | any ((name ==) . fst3) bindings ->
          -- This let shadows our name, so don't recurse any further.
          -- However we still want to apply f to any values bound by the let _before_
          -- the shadowing binding.
          let (bindingsBefore, bindingsAfter) = Data.List.break ((name ==) . fst3) bindings
              bindingsBefore' = traverse (\(n, v, b) -> (n,,b) <$> go v) bindingsBefore
              bindingsAfter' = pure bindingsAfter
              bindings' = liftA2 (<>) bindingsBefore' bindingsAfter'
           in LetT t <$> bindings' <*> pure body
      expr@(IAbsT _ pat _ _)
        | any ((== name) . fst) (fvPat pat) ->
          -- This abstraction shadows our name, so don't recurse further.
          pure expr
      expr ->
        uniplate go expr >>= f

-- | Construct an error for a hole.
newHoleError :: Type -> Name -> Core.Exp Name
newHoleError = undefined

-- | Lift all lambdas in an expression.
-- This returns the resulting expression and a list of new top-level functions
-- that have been generated by lifting.
liftAllLambdas :: forall m. Monad m => m Name -> Exp -> m (Exp, [(Name, [Pattern], Exp)])
liftAllLambdas genName = runWriterT . transformMOf uniplate f
  where
    f :: Exp -> WriterT [(Name, [Pattern], Exp)] m Exp
    f (IAbsT t pat _ e) = do
      name <- lift genName
      let (pats, sub) = liftLambda name t [pat] e
      tell [(name, pats, e)]
      pure sub
    -- TODO: mcase
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
  -- First calculate the parameters of the new function.
  -- These should be, first, any free variables in the body.
  -- Then the patterns of the lambda.
  let boundVars :: [(Name, Type)]
      boundVars = mconcatMap fvPat pats
      freeVars :: [(Name, Type)]
      freeVars = Map.toList (fv body) \\ boundVars
      freeVarsAsPatterns :: [Pattern]
      freeVarsAsPatterns = map (uncurry (flip VarPat)) freeVars
      allPatterns :: [Pattern]
      allPatterns = freeVarsAsPatterns <> pats
      -- Replace any IFn types in the lambda type with Fn types.
      -- This doesn't affect the compiled code but it makes testing easier and
      -- better matches the spirit of things (since implicits are all explicit
      -- at this point).
      functionResultType = foldl (\t _ -> getResultType t) ty boundVars
        where
          getResultType = \case
            TOther (IFn _ t) -> t
            TOther (Fn _ t) -> t
            t -> t
      newFunctionType = foldr (\(_, a) b -> TOther (Fn a b)) functionResultType (freeVars <> boundVars)
      application = foldl f (VarT newFunctionType name) freeVars
        where
          f e (n, t) = AppT (funcTypeRange (typeOf e)) e (VarT t n)
          -- Given a function type `A -> B`, return `B`.
          -- If the type is not a function type, something has gone wrong in the
          -- typechecker so we crash.
          funcTypeRange :: Type -> Type
          funcTypeRange (TOther (Fn _ b)) = b
          funcTypeRange t = error $ "Expected a function type, got " <> debug t
   in (allPatterns, application)

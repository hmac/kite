{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -freduction-depth=300 #-}
module Syn.Typed
  ( Exp
  , Pattern
  , Module(..)
  , Decl(..)
  , Fun(..)
  , Data(..)
  , DataCon(..)
  , applySolution
  , typeOf
  , cacheType
  , AST.Pat(..)
  , AST.ExprT(..)
  , AST.ConMeta(..)
  , Syn.Typed.Implicit
  , AST.Implicit_(..)
  , S.Import(..)
  , Type
  , traverseExprWithLocals
  ) where

import           AST
import           Control.Lens                   ( over
                                                , set
                                                , traverseOf
                                                , view
                                                )
import           Control.Lens.Plated            ( transformMOf
                                                , transformOf
                                                )
import           Data.Data.Lens                 ( uniplate )
import           Data.Generics.Product          ( Param(..)
                                                , param
                                                )
import           Data.Generics.Product.Positions
                                                ( position )
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Name
import qualified Syn                           as S
import           Type.Type                      ( E
                                                , Type(..)
                                                , Type'(..)
                                                )

-- This module contains the typed AST
-- Any module that deals with the typed AST can just import this one to get all
-- the right type definitions.
-- We re-use any types that are unchanged from the untyped AST to avoid
-- pointless conversions.

type Exp = ExprT Name Type
type Pattern = AST.Pat Type Name
type Implicit = AST.Implicit_ Name Type

data Module = Module
  { moduleName    :: PkgModuleName
  , moduleImports :: [S.Import]
  -- TODO: why is this different from 'Syn.moduleExports'?
  , moduleExports :: [Name]
  , moduleDecls   :: [Decl]
  }
  deriving (Eq, Show)

-- foo x (y : ys) (a, b, c) = ...
-- foo x []       _         = ...
data Decl = FunDecl Fun
          | DataDecl Data
            deriving (Eq, Show)

data Fun = Fun
  { funName   :: Name
  , funType   :: Type
  , funExpr   :: Exp
  , funWheres :: [Fun]
  }
  deriving (Eq, Show)

data Data = Data
  { dataName   :: Name
  , dataTyVars :: [Name]
  , dataCons   :: [DataCon]
  }
  deriving (Eq, Show)

data DataCon = DataCon
  { conName :: Name
  , conType :: Type
  , conMeta :: ConMeta
  }
  deriving (Eq, Show)

-- | Apply a set of existential variable solutions to the type annotations of an expression.
-- This is used after typechecking to resolve any existentials in the cached types.
applySolution :: Map E Type -> Exp -> Exp
applySolution s = over (param @0) (transformOf uniplate (solve s))

-- Apply a "solution" - i.e. a map of existential variables to their substitutions - to a type.
-- If the existential solves to another existential or a type containing existentials, we try to
-- solve those as well.
solve :: Map E Type -> Type -> Type
solve s = transformOf uniplate solve'
 where
  solve' :: Type -> Type
  solve' (TApp f xs) = case trySolve f of
    Left  (TApp g ys) -> TApp g (ys <> xs)
    Left  (TOther f') -> TApp f' xs
    Left  (TCon t ys) -> TCon t (ys <> xs)
    Right t           -> TApp t xs
  solve' (TCon t xs) = TCon t xs
  solve' (TOther t ) = case trySolve t of
    Left  t' -> t'
    Right t' -> TOther t'
  -- Apply the solution to the given 'Type''.
  -- Returns either a 'Type' in 'Left' or a 'Type'' in 'Right'
  trySolve :: Type' -> Either Type Type'
  trySolve = transformMOf uniplate go
   where
    go :: Type' -> Either Type Type'
    go (EType e) = maybe (Right (EType e)) (Left . solve s) (Map.lookup e s)
    go t         = Right t

-- | Get the cached type of an 'Exp'
typeOf :: Exp -> Type
typeOf = view (position @1)

-- | Store a cached type on an 'Exp'
cacheType :: Type -> Exp -> Exp
cacheType = set (position @1)

-- | Walk an expression, visiting each node, collecting bound variables as we go
-- under lambdas or mcases.
traverseExprWithLocals
  :: Monad m
  => ([(Name, Type)] -> Exp -> m Exp)
  -> [(Name, Type)]
  -> Exp
  -> m Exp
traverseExprWithLocals action = go
 where
  go ctx expr = do
    expr' <- case expr of
      AbsT t vars e -> AbsT t vars <$> go (ctx <> NE.toList vars) e
      IAbsT t pat pt e ->
        IAbsT t pat pt <$> go (ctx <> varsBoundByPattern pat) e
      CaseT t e alts ->
        let caseAlt (p, rhs) = do
              rhs' <- go (ctx <> varsBoundByPattern p) rhs
              pure (p, rhs')
        in  CaseT t <$> go ctx e <*> traverse caseAlt alts
      MCaseT t alts ->
        let mCaseAlt (pats, rhs) = do
              let vars = concatMap varsBoundByPattern pats
              rhs' <- go (ctx <> vars) rhs
              pure (pats, rhs')
        in  MCaseT t <$> mapM mCaseAlt alts
      LetT ty binds body -> do
        let vars = map (\(n, e, _) -> (n, typeOf e)) binds
        let ctx' = ctx <> vars
        binds' <- mapM (\(n, e, t) -> (n, , t) <$> go ctx' e) binds
        LetT ty binds' <$> go ctx' body

      e -> traverseOf uniplate (go ctx) e
    action ctx expr'
 -- TODO: this could probably be expressed as a traversal
  varsBoundByPattern :: Pattern -> [(Name, Type)]
  varsBoundByPattern = \case
    VarPat t x            -> [(x, t)]
    ConsPat _ _ _ subpats -> concatMap varsBoundByPattern subpats
    TuplePat _ subpats    -> concatMap varsBoundByPattern subpats
    ListPat  _ subpats    -> concatMap varsBoundByPattern subpats
    WildPat{}             -> []
    IntPat{}              -> []
    CharPat{}             -> []
    BoolPat{}             -> []
    UnitPat{}             -> []
    StringPat{}           -> []

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
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
  , S.Import(..)
  , Type
  ) where

import           AST
import           Control.Lens                   ( over
                                                , set
                                                , view
                                                )
import           Control.Lens.Plated            ( transformOf )
import           Data.Data.Lens                 ( uniplate )
import           Data.Generics.Product          ( Param(..)
                                                , param
                                                )
import           Data.Generics.Product.Positions
                                                ( position )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Name
import qualified Syn                           as S
import           Type.Type                      ( E
                                                , Type(..)
                                                )

-- This module contains the typed AST
-- Any module that deals with the typed AST can just import this one to get all
-- the right type definitions.
-- We re-use any types that are unchanged from the untyped AST to avoid
-- pointless conversions.

type Exp = ExprT Name Type
type Pattern = AST.Pat Name

data Module = Module
  { moduleName    :: PkgModuleName
  , moduleImports :: [S.Import]
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
  , conArgs :: [Type]
  , conType :: Type
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
solve s = transformOf uniplate go
 where
  go (EType e) = maybe (EType e) (solve s) (Map.lookup e s)
  go t         = t

-- | Get the cached type of an 'Exp'
typeOf :: Exp -> Type
typeOf = view (position @1)

-- | Store a cached type on an 'Exp'
cacheType :: Type -> Exp -> Exp
cacheType = set (position @1)

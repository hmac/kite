module Syn.Typed
  ( module Syn.Typed
  , Constraint.Type(..)
  , Constraint.Expr.ExpT(..)
  , Constraint.Expr.AltT(..)
  , Constraint.Expr.Scheme(..)
  )
where

import qualified Syn                           as S
import           Constraint.Expr                ( ExpT(..)
                                                , AltT(..)
                                                , Scheme(..)
                                                )
import           Constraint                     ( Type(..) )
import           Canonical                      ( Name )


-- This module contains type aliases for the typed AST
-- Any module that deals with the typed AST can just import this one to get all
-- the right type definitions.

type Exp = ExpT
type Pattern = S.Pattern_ Name
type DataCon = S.DataCon_ Name
type Instance = S.Instance_ Name Exp
type Typeclass = S.Typeclass_ Name
type Data = S.Data_ Name
type Def = S.Def_ Name Exp
type Fun = S.Fun_ Name Exp Scheme
type Constraint = S.Constraint_ Name
type Decl = S.Decl_ Name Exp Scheme
type Import = S.Import_ Name
type Module = S.Module_ Name Exp Scheme

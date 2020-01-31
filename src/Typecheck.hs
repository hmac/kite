module Typecheck where

-- This module takes types from Syntax and infers types for them, producing
-- annotated equivalents or a type error.

import           Syntax                  hiding ( Name )
import           Canonical                      ( Name(..) )
import           Constraint                     ( Error )
import           Constraint.Generate.M          ( run )
import           Constraint.Generate.Bind       ( BindT(..) )
import           Constraint.Generate.Module     ( generateModule )

import qualified Constraint.Primitive

inferModule :: Module_ Name (Syn_ Name) -> Either Error [BindT]
inferModule modul = fst $ run (generateModule Constraint.Primitive.env modul)

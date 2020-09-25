module Syn.Typed
  ( module Syn.Typed
  , Constraint.Type(..)
  , Constraint.Expr.ExpT(..)
  , Constraint.Expr.AltT(..)
  , Constraint.Expr.Scheme
  , S.Pattern_(..)
  , S.Import(..)
  , Scheme_(..)
  )
where

import           Data.Name
import qualified Syn                           as S
import           Constraint.Expr                ( ExpT(..)
                                                , AltT(..)
                                                , Scheme
                                                , Scheme_(..)
                                                )
import           Constraint                     ( Type(..) )

-- This module contains the typed AST
-- Any module that deals with the typed AST can just import this one to get all
-- the right type definitions.
-- We re-use any types that are unchanged from the untyped AST to avoid
-- pointless conversions.

-- Constraint.Expr.ExpT reuses the Pattern type from Syn, so we do the same
type Exp = ExpT
type Pattern = S.Pattern_ Name

data Module = Module { moduleName :: ModuleName
                     , moduleImports :: [S.Import]
                     , moduleExports :: [Name]
                     , moduleDecls :: [Decl]
                     } deriving (Eq, Show)

-- foo x (y : ys) (a, b, c) = ...
-- foo x []       _         = ...
data Decl = FunDecl Fun
          | DataDecl Data
            deriving (Eq, Show)

data Fun = Fun { funName :: Name
               , funType :: Scheme
               , funExpr :: Exp
               } deriving (Eq, Show)

data Data = Data { dataName :: Name
                 , dataTyVars :: [Name]
                 , dataCons :: [DataCon]
                 } deriving (Eq, Show)

data DataCon = DataCon { conName :: Name, conArgs :: [Type], conType :: Scheme }
             | RecordCon { conName :: Name, conFields :: [(Name, Type)], conType :: Scheme }
               deriving (Eq, Show)

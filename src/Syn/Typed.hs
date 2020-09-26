module Syn.Typed
  ( module Syn.Typed
  , AST.Pat(..)
  , AST.ExprT(..)
  , S.Import(..)
  )
where

import           Data.Name
import           Type                           ( Type )
import qualified Syn                           as S
import           AST

-- This module contains the typed AST
-- Any module that deals with the typed AST can just import this one to get all
-- the right type definitions.
-- We re-use any types that are unchanged from the untyped AST to avoid
-- pointless conversions.

type Exp = ExprT Name Type
type Pattern = AST.Pat Name

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
               , funType :: Type
               , funExpr :: Exp
               } deriving (Eq, Show)

data Data = Data { dataName :: Name
                 , dataTyVars :: [Name]
                 , dataCons :: [DataCon]
                 } deriving (Eq, Show)

data DataCon = DataCon { conName :: Name, conArgs :: [Type], conType :: Type }
             | RecordCon { conName :: Name, conFields :: [(Name, Type)], conType :: Type }
               deriving (Eq, Show)

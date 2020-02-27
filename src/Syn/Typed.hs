module Syn.Typed
  ( module Syn.Typed
  , Constraint.Type(..)
  , Constraint.Expr.ExpT(..)
  , Constraint.Expr.AltT(..)
  , Constraint.Expr.Scheme(..)
  , S.Pattern_(..)
  , S.Import_(..)
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
import           Canonical                      ( Name )
import           Util


-- This module contains the typed AST
-- Any module that deals with the typed AST can just import this one to get all
-- the right type definitions.
-- We re-use any types that are unchanged from the untyped AST to avoid
-- pointless conversions.

-- Constraint.Expr.ExpT reuses the Pattern type from Syn, so we do the same
type Exp = ExpT
type Pattern = S.Pattern_ Name

data Module = Module { moduleName :: ModuleName
                     , moduleImports :: [Import]
                     , moduleExports :: [Name]
                     , moduleDecls :: [Decl]
                     } deriving (Eq, Show)

typeclassDecls :: Module -> [Typeclass]
typeclassDecls Module { moduleDecls = decls } = flip mapMaybe decls $ \case
  TypeclassDecl t -> Just t
  _               -> Nothing

instanceDecls :: Module -> [Instance]
instanceDecls Module { moduleDecls = decls } = flip mapMaybe decls $ \case
  TypeclassInst i -> Just i
  _               -> Nothing

-- import Bar
-- import qualified Baz as Boo (fun1, fun2)
type Import = S.Import

-- foo x (y : ys) (a, b, c) = ...
-- foo x []       _         = ...
data Decl = FunDecl Fun
          | DataDecl Data
          | TypeclassDecl Typeclass
          | TypeclassInst Instance
            deriving (Eq, Show)

data Fun = Fun { funName :: Name
               , funType :: Scheme
               , funDefs :: [Def]
               } deriving (Eq, Show)

data Data = Data { dataName :: Name
                 , dataTyVars :: [Name]
                 , dataCons :: [DataCon]
                 } deriving (Eq, Show)

data DataCon = DataCon { conName :: Name, conArgs :: [Type], conType :: Scheme }
             | RecordCon { conName :: Name, conFields :: [(Name, Type)], conType :: Scheme }
               deriving (Eq, Show)

data Typeclass = Typeclass { typeclassName :: Name
                           , typeclassTyVars :: [Name]
                           , typeclassDefs :: [(Name, Type)]
                           } deriving (Eq, Show)

data Instance = Instance { instanceName :: Name
                         , instanceTypes :: [Type]
                         -- TODO: instanceDefs :: [Fun] ?
                         , instanceDefs :: [(Name, Scheme, [Def])]
                         } deriving (Eq, Show)

data Def = Def { defArgs :: [Pattern] , defExpr :: Exp }
           deriving (Eq, Show)

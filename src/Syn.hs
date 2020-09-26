module Syn
  ( Module
  , Module_(..)
  , dataDecls
  , funDecls
  , extractDecl
  , Import(..)
  , ImportItem
  , ImportItem_(..)
  , unName
  , Decl
  , Decl_(..)
  , Fun
  , Fun_(..)
  , Data
  , Data_(..)
  , Alias
  , Alias_(..)
  , DataCon
  , DataCon_(..)
  , Pattern
  , tyapp
  , fn
  , ftv
  , binOps
  , Syn
  , Type
  , Type_(..)
  , module Data.Name
  )
where

import qualified Data.Set                      as Set
import           Data.Name                      ( ModuleName(..)
                                                , RawName(..)
                                                )
import           Util

import           Type.Reflection                ( Typeable )
import qualified Data.Data                     as Data
import qualified AST                            ( Expr
                                                , Pat
                                                )

-- module Foo
type Module = Module_ RawName Syn Type
data Module_ name a ty = Module { moduleName :: ModuleName
                                , moduleImports :: [Import]
                                , moduleExports :: [(name, [name])]
                                , moduleDecls :: [Decl_ name a ty]
                                , moduleMetadata :: [(String, String)]
                                }
                                deriving (Eq, Show, Typeable, Data.Data)

dataDecls :: Module_ n a ty -> [Data_ n]
dataDecls = extractDecl $ \case
  DataDecl d -> Just d
  _          -> Nothing

funDecls :: Module_ n a ty -> [Fun_ n a ty]
funDecls = extractDecl $ \case
  FunDecl f -> Just f
  _         -> Nothing

extractDecl :: (Decl_ n e ty -> Maybe b) -> Module_ n e ty -> [b]
extractDecl f Module { moduleDecls = decls } = mapMaybe f decls

-- import Bar
-- import qualified Baz as Boo (fun1, fun2)
data Import = Import { importQualified :: Bool
                     , importName :: ModuleName
                     , importAlias :: Maybe RawName
                     , importItems :: [ImportItem_ RawName]
                     }
                     deriving (Eq, Show, Typeable, Data.Data)

type ImportItem = ImportItem_ RawName
data ImportItem_ name = ImportSingle { importItemName :: name }
                      | ImportAll { importItemName :: name }
                      | ImportSome { importItemName :: name, importItemConstructors :: [name] }
                      deriving (Eq, Show, Typeable, Data.Data)

unName :: RawName -> String
unName (Name n) = n

-- foo x (y : ys) (a, b, c) = ...
-- foo x []       _         = ...
type Decl a = Decl_ RawName a Type
data Decl_ name exp ty = FunDecl (Fun_ name exp ty)
                       | DataDecl (Data_ name)
                       | AliasDecl (Alias_ name)
                       | Comment String
                       deriving (Eq, Show, Typeable, Data.Data)

type Fun exp = Fun_ RawName exp (Type_ RawName)
data Fun_ name exp ty = Fun { funComments :: [String]
                            , funName :: name
                            , funType :: Maybe ty
                            , funExpr :: exp
                            }
                            deriving (Eq, Show, Typeable, Data.Data)

type Data = Data_ RawName
data Data_ name = Data { dataName :: name
                       , dataTyVars :: [RawName]
                       , dataCons :: [DataCon_ name]
                       }
                       deriving (Eq, Show, Typeable, Data.Data)

-- A type alias in its raw form. The variables in aliasTyVars are free in the
-- type. This is later converted into a Scheme, so they get quantified over
-- properly.
type Alias = Alias_ RawName
data Alias_ name = Alias { aliasName :: name
                         , aliasTyVars :: [RawName]
                         , aliasType :: Type_ name
                         }
                         deriving (Eq, Show, Typeable, Data.Data)

type DataCon = DataCon_ RawName
-- Left a
-- Foo { unFoo : a, label : String }
data DataCon_ name = DataCon { conName :: name
                             , conArgs :: [Type_ name]
                             }
                             deriving (Eq, Show, Typeable, Data.Data)

-- TODO: record patterns
type Pattern = AST.Pat RawName

-- Int
-- Maybe Int
-- a
-- Int -> String
-- a -> b
-- f a
-- { a : A, b : B }
type Type = Type_ RawName
data Type_ a =
          TyCon a
        | TyApp (Type_ a) (Type_ a)
        | TyVar a
        | TyList
        | TyTuple [Type_ a]
        | TyHole RawName
        | TyInt
        | TyString
        | TyChar
        -- Bool could be defined in library code, but it's a lot easier to write
        -- primitives like $eqInt if it's a built-in type.
        | TyBool
        | TyUnit
        | TyFun (Type_ a) (Type_ a)
        | TyRecord [(a, Type_ a)]
        | TyAlias a (Type_ a)
        | TyForall a (Type_ a)
  deriving (Eq, Show, Ord, Typeable, Data.Data)

infixl 5 `tyapp`
tyapp :: Type_ name -> Type_ name -> Type_ name
tyapp = TyApp

infixr 4 `fn`
fn :: Type_ name -> Type_ name -> Type_ name
fn = TyFun

-- Get the free type variables of a type
ftv :: Ord name => Type_ name -> Set.Set name
ftv = \case
  TyVar x         -> Set.singleton x
  TyApp a b       -> ftv a <> ftv b
  TyTuple as      -> mconcat (map ftv as)
  TyFun a b       -> ftv a <> ftv b
  TyRecord fields -> mconcat $ map (ftv . snd) fields
  TyAlias _ a     -> ftv a
  _               -> mempty

-- Syn: the surface syntax
-- Syn represents the code that users write. It goes through several
-- translations before being executed.
--
-- Immediately after a module is loaded (in ModuleLoader) it is canonicalised,
-- a process that qualifies all names with their full module path. This produces
-- a Canonical.Exp which is another name for Syn (Canonical.Name).
--
-- For typechecking we convert Syn to Type.Exp and typecheck that,
-- producing Syn.Typed. This is then converted to ELC and then to LC for
-- evaluation.
--
-- [Syn] -> [Can.Exp] -> [Type.Exp] -> [Syn.Typed] -> [ELC] -> [LC]
type Syn = AST.Expr RawName Type

-- Supported binary operators
binOps :: [RawName]
binOps = ["+", "-", "*", "/", ">", "<", "&&", "||", ">=", "<=", "<>"]

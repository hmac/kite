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
  , Constraint
  , Constraint_(..)
  , Data
  , Data_(..)
  , Alias
  , Alias_(..)
  , DataCon
  , DataCon_(..)
  , Pattern
  , Pattern_(..)
  , tyapp
  , fn
  , ftv
  , Scheme
  , Scheme_(..)
  , binOps
  , Syn
  , Syn_(..)
  , Type
  , Type_(..)
  , module Data.Name
  )
where

import           Data.List                      ( intercalate )
import qualified Data.Set                      as Set
import           Data.Name                      ( ModuleName(..)
                                                , RawName(..)
                                                )
import           Util

import           Type.Reflection                ( Typeable )
import qualified Data.Data                     as Data

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

-- TODO: I think it'll simplify things down the line if we have a single
-- funScheme field that contains both the constraint and the type, instead of
-- splitting them. This is because after type inference we get a Scheme, and
-- splitting it back out into constraint and type (with free variables) seems a
-- bit pointless.
type Fun exp = Fun_ RawName exp (Type_ RawName)
data Fun_ name exp ty = Fun { funComments :: [String]
                            , funName :: name
                            , funType :: Maybe ty
                            , funExpr :: exp
                            }
                            deriving (Eq, Show, Typeable, Data.Data)

-- Constraints
-- Should be removed as we no longer have typeclasses
type Constraint = Constraint_ RawName
data Constraint_ name =
    CTuple (Constraint_ name) (Constraint_ name)
  | CNil
  deriving (Show, Eq)

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
type Pattern = Pattern_ RawName
data Pattern_ a = VarPat a
             | WildPat
             | IntPat Int
             | CharPat Char
             | BoolPat Bool
             | UnitPat
             | TuplePat [Pattern_ a]
             | ListPat [Pattern_ a]
             | ConsPat a [Pattern_ a]
             | StringPat String
             deriving (Eq, Show, Typeable, Data.Data)

instance Debug a => Debug (Pattern_ a) where
  debug (VarPat v)       = debug v
  debug WildPat          = "_"
  debug (IntPat  i)      = show i
  debug (CharPat c)      = "'" <> [c] <> "'"
  debug (BoolPat b)      = show b
  debug UnitPat          = "()"
  debug (TuplePat args ) = "(" <> sepBy ", " (map debug args) <> ")"
  debug (ListPat  args ) = "[" <> sepBy ", " (map debug args) <> "]"
  debug (ConsPat c args) = "(" <> debug c <+> sepBy " " (map debug args) <> ")"
  debug (StringPat s   ) = "\"" <> s <> "\""

sepBy :: String -> [String] -> String
sepBy = intercalate

(<+>) :: String -> String -> String
a <+> b = a <> " " <> b

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

-- Type schemes
-- v: the type of type variables
-- c: the type of constraints
-- t: the type of types
type Scheme = Scheme_ RawName Type
data Scheme_ v t = Forall [v] t
  deriving (Eq, Show, Ord, Typeable, Data.Data)

-- Syn: the surface syntax
-- Syn represents the code that users write. It goes through several
-- translations before being executed.
--
-- Immediately after a module is loaded (in ModuleLoader) it is canonicalised,
-- a process that qualifies all names with their full module path. This produces
-- a Canonical.Exp which is another name for Syn (Canonical.Name).
--
-- For typechecking we convert Syn to Constraint.Exp and typecheck that,
-- producing Constraint.ExpT. This is then converted to ELC and then to LC for
-- evaluation.
--
-- [Syn] -> [Can.Exp] -> [Constraint.Exp] -> [Constraint.ExpT] -> [ELC] -> [LC]

-- TODO: patterns in let bindings
-- TODO: type sigs in let bindings
-- TODO: multi-definition functions in let bindings
--       (e.g. let fib 0 = 1; fib 1 = 1; fib n = ...)
type Syn = Syn_ RawName RawName Type
data Syn_ n v t = Var n
         | Con n
         | Hole n
         | Abs [n] (Syn_ n v t)
         | App (Syn_ n v t) (Syn_ n v t)
         -- Note: the parser can't currently produce LetAs but the typechecker
         -- can nonetheless handle them.
         | LetA n (Scheme_ v t) (Syn_ n v t) (Syn_ n v t)
         | Let [(n, Syn_ n v t)] (Syn_ n v t)
         | Case (Syn_ n v t) [(Pattern_ n, Syn_ n v t)]
         | MCase [([Pattern_ n], Syn_ n v t)]
         | UnitLit
         | TupleLit [Syn_ n v t]
         | ListLit [Syn_ n v t]
         | StringLit String [(Syn_ n v t, String)]
         | CharLit Char
         | IntLit Int
         | BoolLit Bool
         -- Records
         | Record [(n, Syn_ n v t)]
         | Project (Syn_ n v t) n
         -- FFI calls
         -- FCall stands for "foreign call"
         | FCall String [Syn_ n v t]
         deriving (Eq, Show, Typeable, Data.Data)

-- Supported binary operators
binOps :: [RawName]
binOps = ["+", "-", "*", "/", ">", "<", "&&", "||", ">=", "<=", "<>"]

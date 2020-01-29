{-# LANGUAGE DerivingVia #-}
module Syntax
  ( module Syntax
  , module Data.Name
  )
where

import           Data.Name                      ( ModuleName(..)
                                                , RawName(..)
                                                )
import           Util

-- module Foo
type Module = Module_ Name
data Module_ name a = Module { moduleName :: ModuleName
                             , moduleImports :: [Import]
                             , moduleExports :: [name]
                             , moduleDecls :: [Decl_ name a]
                             , moduleMetadata :: [(String, String)]
                             }
                             deriving (Eq, Show)

modifyModuleDecls :: (Decl a -> Decl a) -> Module a -> Module a
modifyModuleDecls f m = m { moduleDecls = map f (moduleDecls m) }

typeclassDecls :: Module_ n a -> [Typeclass_ n]
typeclassDecls Module { moduleDecls = decls } = flip mapMaybe decls $ \case
    TypeclassDecl t -> Just t
    _               -> Nothing

instanceDecls :: Module_ n e -> [Instance_ n e]
instanceDecls Module { moduleDecls = decls } = flip mapMaybe decls $ \case
  TypeclassInst i -> Just i
  _ -> Nothing

-- import Bar
-- import qualified Baz as Boo (fun1, fun2)
type Import = Import_ Name
data Import_ name = Import { importQualified :: Bool
                           , importName :: ModuleName
                           , importAlias :: Maybe Name
                           , importItems :: [name]
                           }
                           deriving (Eq, Show)

-- Variable name
-- foo
type Name = RawName

unName :: Name -> String
unName (Name n) = n

-- foo x (y : ys) (a, b, c) = ...
-- foo x []       _         = ...
type Decl a = Decl_ Name a
data Decl_ name exp = FunDecl (Fun_ name exp)
                    | DataDecl (Data_ name)
                    | TypeclassDecl (Typeclass_ name)
                    | TypeclassInst (Instance_ name exp)
                    | Comment String
                  deriving (Eq, Show)

type Fun exp = Fun_ Name exp
data Fun_ name exp = Fun { funComments :: [String]
                         , funName :: name
                         , funType :: Ty_ name
                         , funConstraint :: Maybe (Constraint_ name)
                         , funDefs :: [Def_ name exp]
                         }
                         deriving (Eq, Show)

-- Typeclass constaints
-- Monoid a => ...
-- (Applicative a, Alternative b) => ...
type Constraint = Constraint_ Name
data Constraint_ name = CInst name [Ty_ name]
                | CTuple (Constraint_ name) (Constraint_ name)
                deriving (Show, Eq)

type Data = Data_ Name
data Data_ name = Data { dataName :: name
                       , dataTyVars :: [Name]
                       , dataCons :: [DataCon_ name]
                       }
                       deriving (Eq, Show)

-- TODO: default definitions
-- TODO: we probably want to error if you put type holes in typeclass/instance
-- defs
type Typeclass = Typeclass_ Name
data Typeclass_ name = Typeclass { typeclassName :: name
                                 , typeclassTyVars :: [name] -- should this be [Name]?
                                 , typeclassDefs :: [(name, Ty_ name)]
                                 }
                                 deriving (Eq, Show)

-- TODO: instance constraints
-- e.g. instance (Eq a, Eq b) => Eq (a, b)
--               ^^^^^^^^^^^^^^^
type Instance exp = Instance_ Name exp
data Instance_ name exp = Instance { instanceName :: name
                                   , instanceTypes :: [Ty_ name]
                                   , instanceDefs :: [(name, [Def_ name exp])]
                                   }
                                   deriving (Eq, Show)

type DataCon = DataCon_ Name
-- Left a
-- Foo { unFoo : a, label : String }
data DataCon_ name = DataCon { conName :: name
                             , conArgs :: [Ty_ name]
                             }
                   | RecordCon { conName :: name
                               , conFields :: [(name, Ty_ name)]
                               } 
                             deriving (Eq, Show)

-- Consider adding defName here - I think it might simplify things elsewhere
type Def = Def_ Name
data Def_ name a = Def { defArgs :: [Pattern_ name]
                       , defExpr :: a
                       }
                       deriving (Eq, Show)

type Pattern = Pattern_ Name
data Pattern_ a = VarPat a
             | WildPat
             | IntPat Int
             | TuplePat [Pattern_ a]
             | ListPat [Pattern_ a]
             | ConsPat a [Pattern_ a]
             | StringPat String
             deriving (Eq, Show)

-- Int
-- Maybe Int
-- a
-- Int -> String
-- a -> b
-- f a
-- TODO: rename to Type?
-- TODO: I think this is a shitty way to structure type application - hard to
-- use in practice. The approach in Constraint.Expr.Type is a lot better.
type Ty = Ty_ Name
data Ty_ a = Ty_ a :@: Ty_ a
        | TyArr
        | TyCon a
        | TyVar a
        | TyList (Ty_ a)
        | TyTuple [Ty_ a]
        | TyHole Name
        | TyInt
        | TyString
  deriving (Eq, Show, Ord)

infixr 4 `fn`
fn :: Ty_ name -> Ty_ name -> Ty_ name
a `fn` b = (TyArr :@: a) :@: b

-- Syn: the surface syntax
-- Syn represents the code that users write. It goes through several
-- translations before being executed.

-- Immediately after a module is loaded (in ModuleLoader) it is canonicalised,
-- a process that qualifies all names with their full module path. This produces
-- a Canonical.Exp which is another name for Syn (Canonical.Name).

-- For typechecking we convert Syn to Core and typecheck the Core.
-- For evaluation we convert Syn to ELC, then to LC for evaluation.

type Syn = Syn_ Name
data Syn_ name = Var name
         | Cons name
         | Hole name
         | Abs [name] (Syn_ name)
         | App (Syn_ name) (Syn_ name)
         -- TODO: patterns in let bindings
         -- TODO: type sigs in let bindinds
         -- TODO: multi-definition functions in let bindings
         --       (e.g. let fib 0 = 1; fib 1 = 1; fib n = ...)
         | LetA name (Ty_ name) (Syn_ name) (Syn_ name)
         | Let [(name, Syn_ name)] (Syn_ name)
         | Case (Syn_ name) [(Pattern_ name, Syn_ name)]
         -- more exotic syntactic structures
         | TupleLit [Syn_ name]
         | ListLit [Syn_ name]
         | StringLit String [(Syn_ name, String)]
         | IntLit Int
         deriving (Eq, Show)

-- Supported binary operators
binOps :: [Name]
binOps = ["+", "-", "*", "/", ">", "<", "&&", "||", ">=", "<=", "<>"]

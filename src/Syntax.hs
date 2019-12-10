{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
module Syntax where

import           Data.String                    ( IsString(fromString) )
import           GHC.Generics                   ( Generic )
import           Data.Hashable                  ( Hashable )

-- module Foo
data Module a = Module { moduleName :: ModuleName
                     , moduleImports :: [Import]
                     , moduleExports :: [Name]
                     , moduleDecls :: [Decl a]
                     , moduleMetadata :: [(String, String)]
                     }
                     deriving (Eq, Show, Generic)

-- import Bar
-- import qualified Baz as Boo (fun1, fun2)
data Import = Import { importQualified :: Bool
                     , importName :: ModuleName
                     , importAlias :: Maybe Name
                     , importItems :: [Name]
                     }
                     deriving (Eq, Show, Generic)

-- Variable name
-- foo
newtype Name = Name String
  deriving (Eq, Show, Generic)
  deriving Hashable via String

instance IsString Name where
  fromString = Name

newtype ModuleName = ModuleName [String]
  deriving (Eq, Show, Generic)

instance IsString ModuleName where
  fromString s = ModuleName $ splitOn '.' s

-- foo x (y : ys) (a, b, c) = ...
-- foo x []       _         = ...
data Decl a = FunDecl (Fun a)
          | DataDecl Data
          | TypeclassDecl Typeclass
          | TypeclassInst (Instance a)
          | Comment String
        deriving (Eq, Show, Generic)

data Fun a = Fun { funComments :: [String]
               , funName :: Name
               , funType :: Ty
               , funDefs :: [Def a]
               }
               deriving (Eq, Show, Generic)

data Data = Data { dataName :: Name
                 , dataTyVars :: [Name]
                 , dataCons :: [DataCon]
                 }
                 deriving (Eq, Show, Generic)

-- TODO: default definitions
-- TODO: we probably want to error if you put type holes in typeclass/instance
-- defs
data Typeclass = Typeclass { typeclassName :: Name
                           , typeclassTyVars :: [Name]
                           , typeclassDefs :: [(Name, Ty)]
                           }
                           deriving (Eq, Show, Generic)

data Instance a = Instance { instanceName :: Name
                         , instanceTypes :: [Ty]
                         , instanceDefs :: [(Name, [Def a])]
                         }
                         deriving (Eq, Show, Generic)

data DataCon = DataCon { conName :: Name
                       , conArgs :: [Ty]
                       }
                       deriving (Eq, Show, Generic)

-- Consider adding defName here - I think it might simplify things elsewhere
data Def a = Def { defArgs :: [Pattern]
                 , defExpr :: a
                 }
                 deriving (Eq, Show, Generic)

data Pattern = VarPat Name            -- x
             | WildPat                -- _
             | IntPat Int             -- 1
             | TuplePat [Pattern]     -- (x, y)
             | ListPat [Pattern]      -- [x, y]
             | ConsPat Name [Pattern] -- Just x, x : xs, Nothing, True
             deriving (Eq, Show, Generic)

-- Int
-- Maybe Int
-- a
-- Int -> String
-- a -> b
-- f a
-- TODO: rename to Type?
data Ty = Ty :@: Ty
        | TyArr
        | TyCon Name
        | TyVar Name
        | TyList Ty
        | TyTuple [Ty]
        | TyHole Name
        | TyInt
        | TyFloat
        | TyString
        deriving (Eq, Show, Generic)

infixr 4 `fn`
fn :: Ty -> Ty -> Ty
a `fn` b = (TyArr :@: a) :@: b

-- Syn: the surface syntax
-- Syn represents the code that users write. It goes through several
-- translations before being executed.

-- For typechecking we convert Syn to Core and typechecking the Core.
-- For evaluation we convert Syn to ELC.
-- We currently evaluate ELC directly but in future ELC will be converted to LC
-- before evaluation.

data Syn = Var Name
         | Cons Name
         | Hole Name
         | Abs [Name] Syn
         | App Syn Syn
         -- TODO: patterns in let bindings
         -- TODO: type sigs in let bindinds
         -- TODO: multi-definition functions in let bindings
         --       (e.g. let fib 0 = 1; fib 1 = 1; fib n = ...)
         | Let [(Name, Syn)] Syn
         | Case Syn [(Pattern, Syn)]
         -- more exotic syntactic structures
         | TupleLit [Syn]
         | ListLit [Syn]
         | StringLit String [(Syn, String)]
         | IntLit Int
         | FloatLit Float
         deriving (Eq, Show, Generic)

-- Supported binary operators
binOps :: [Name]
binOps = ["+", "-", "*", "/", ">", "<", "&&", "||", ">=", "<="]

-- Utils

-- | Split a list into sublists delimited by the given element.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
 where
  go []       acc = [reverse acc]
  go (y : ys) acc = if x == y then reverse acc : go ys [] else go ys (y : acc)

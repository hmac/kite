{-# LANGUAGE DeriveGeneric #-}
module Syntax where

import           Data.String                    ( IsString(fromString) )
import           GHC.Generics                   ( Generic )

-- module Foo
data Module = Module { moduleName :: ModuleName
                     , moduleImports :: [Import]
                     , moduleExports :: [Name]
                     , moduleDecls :: [Decl]
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

instance IsString Name where
  fromString = Name

newtype ModuleName = ModuleName [String]
  deriving (Eq, Show, Generic)

instance IsString ModuleName where
  fromString s = ModuleName $ splitOn '.' s

-- foo x (y : ys) (a, b, c) = ...
-- foo x []       _         = ...
-- TODO: records
data Decl = FunDecl Fun | DataDecl Data | TypeclassDecl Typeclass | TypeclassInst Instance
        deriving (Eq, Show, Generic)

data Fun = Fun { funName :: Name
               , funType :: Ty
               , funDefs :: [Def]
               }
               deriving (Eq, Show, Generic)

data Data = Data { dataName :: Name
                 , dataTyVars :: [Name]
                 , dataCons :: [DataCon]
                 }
                 deriving (Eq, Show, Generic)

-- TODO: default definitions
data Typeclass = Typeclass { typeclassName :: Name
                           , typeclassTyVars :: [Name]
                           , typeclassDefs :: [(Name, Ty)]
                           }
                           deriving (Eq, Show, Generic)

data Instance = Instance { instanceName :: Name
                         , instanceTypes :: [Ty]
                         , instanceDefs :: [(Name, [Def])]
                         }
                         deriving (Eq, Show, Generic)

data DataCon = DataCon { conName :: Name
                       , conArgs :: [Ty]
                       }
                       deriving (Eq, Show, Generic)

data Def = Def { defArgs :: [Pattern]
               , defExpr :: Syn
               }
               deriving (Eq, Show, Generic)

data Pattern = VarPat Name            -- x
             | WildPat                -- _
             | LitPat Literal         -- 1, "hi", 3.14
             | TuplePat [Pattern]     -- (x, y)
             | ListPat [Pattern]      -- [x, y]
             | ConsPat Name [Pattern] -- Just x, x : xs, Nothing, True
             deriving (Eq, Show, Generic)

-- 1
-- 3.14
-- "hi"
data Literal = LitInt Int
             | LitFloat Float
             | LitString String
             deriving (Eq, Show, Generic)

-- Int
-- Maybe Int
-- a
-- Int -> String
-- a -> b
-- f a
data Ty = TyApp Name [Ty]
        | TyVar Name
        | TyArr Ty Ty
        | TyList Ty
        | TyTuple [Ty]
        deriving (Eq, Show, Generic)

data Syn = Var Name
         | Cons Name
         | Abs [Name] Syn
         | App Syn Syn
         | Let [(Name, Syn)] Syn
         | Case Syn [(Pattern, Syn)]
         -- more exotic syntactic structures
         | TupleLit [Syn]
         | ListLit [Syn]
         | Lit Literal
         deriving (Eq, Show, Generic)

-- Utils

-- | Split a list into sublists delimited by the given element.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
 where
  go []       acc = [reverse acc]
  go (y : ys) acc = if x == y then reverse acc : go ys [] else go ys (y : acc)

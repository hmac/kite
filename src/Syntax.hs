module Syntax where

import           Data.String                    ( IsString(fromString) )

-- module Foo
-- TODO: moduleName should be global (e.g. Data.Foo.Bar)
data Module = Module { moduleName :: String
                     , moduleImports :: [Import]
                     , moduleExports :: [Name]
                     , moduleDecls :: [Decl]
                     , moduleMetadata :: [(String, String)]
                     }
                     deriving (Eq, Show)

-- import Bar
-- import qualified Baz as Boo (fun1, fun2)
data Import = Import { importQualified :: Bool
                     , importName :: ModuleName
                     , importAlias :: Maybe Name
                     , importItems :: [Name]
                     }
                     deriving (Eq, Show)

-- foo
newtype Name = Name String
        deriving (Eq, Show)

instance IsString Name where
  fromString = Name

newtype ModuleName = ModuleName [String]
        deriving (Eq, Show)

-- foo x (y : ys) (a, b, c) = ...
-- foo x []       _         = ...
-- TODO: records
data Decl = FunDecl Fun | DataDecl Data | TypeclassDecl Typeclass | TypeclassInst Instance
        deriving (Eq, Show)

data Fun = Fun { funName :: Name
               , funType :: Ty
               , funDefs :: [Def]
               }
               deriving (Eq, Show)

data Data = Data { dataName :: Name
                 , dataTyVars :: [Name]
                 , dataCons :: [DataCon]
                 }
                 deriving (Eq, Show)

-- TODO: default definitions
data Typeclass = Typeclass { typeclassName :: Name
                           , typeclassTyVars :: [Name]
                           , typeclassDefs :: [(Name, Ty)]
                           }
                           deriving (Eq, Show)

data Instance = Instance { instanceName :: Name
                         , instanceTypes :: [Ty]
                         , instanceDefs :: [(Name, [Def])]
                         }
                         deriving (Eq, Show)

data DataCon = DataCon { conName :: Name
                       , conArgs :: [Ty]
                       }
                       deriving (Eq, Show)

data Def = Def { defArgs :: [Pattern]
               , defExpr :: Syn
               }
               deriving (Eq, Show)

data Pattern = VarPat Name            -- x
             | WildPat                -- _
             | LitPat Literal         -- 1, "hi", 3.14
             | TuplePat [Pattern]     -- (x, y)
             | ListPat [Pattern]      -- [x, y]
             | ConsPat Name [Pattern] -- Just x, x : xs, Nothing, True
             deriving (Eq, Show)

-- 1
-- 3.14
-- "hi"
data Literal = LitInt Int
             | LitFloat Float
             | LitString String
             deriving (Eq, Show)

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
        deriving (Eq, Show)

-- TODO: case
data Syn = Var Name
         | Cons Name
         | Abs [Name] Syn
         | App Syn Syn
         | Let [(Name, Syn)] Syn
         -- more exotic syntactic structures
         | TupleLit [Syn]
         | ListLit [Syn]
         | Lit Literal
         deriving (Eq, Show)

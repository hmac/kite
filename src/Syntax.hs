module Syntax where

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
newtype ModuleName = ModuleName [String]
        deriving (Eq, Show)

-- Boo.fun1
data GlobalName = LocalName String
                | QualifiedName ModuleName String
                deriving (Eq, Show)

-- foo x (y : ys) (a, b, c) = ...
-- foo x []       _         = ...
data Decl = Decl { declName :: Name
                 , declType :: Ty
                 , declDefs :: [Def]
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
data Ty = TyCon Name [Ty]
        | TyVar Name
        | TyArr Ty Ty
        | TyList Ty
        | TyTuple [Ty]
        deriving (Eq, Show)

data Syn = Var Name
         | Abs [Name] Syn
         | App Syn Syn
         | Let [(Name, Syn)] Syn
         -- more exotic syntactic structures
         | TupleLit [Syn]
         | ListLit [Syn]
         | Lit Literal
         deriving (Eq, Show)

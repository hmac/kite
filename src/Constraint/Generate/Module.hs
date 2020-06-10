-- | Generate constraints for a whole Lam module
-- This basically involves generating constraints for each bind, accumulating an
-- environment as we go.
-- We will need an initial environment containing (possibly unknown) types for
-- any imported items.
module Constraint.Generate.Module where

import qualified Data.Map.Strict               as Map

import           Constraint.Generate.M
import qualified Syn.Typed                     as T
import           Syn                     hiding ( Name
                                                , fn
                                                )
import           Data.Name
import qualified Canonical                     as Can
import           Constraint
import           Constraint.Expr                ( Exp
                                                , ExpT
                                                )
import           Constraint.FromSyn             ( fromSyn
                                                , tyToScheme
                                                )
import           Constraint.Generate.Bind
import qualified Constraint.Generate.Data
import           Util

generateModule
  :: TypeEnv -> Can.Module -> GenerateM LocatedError (TypeEnv, T.Module)
generateModule env modul = do
  -- Extract data declarations
  let datas = map Constraint.Generate.Data.translate
                  (getDataDecls (moduleDecls modul))
      env' = env <> mconcat (map Constraint.Generate.Data.generate datas)

  -- TODO: check that all types are in scope/exist

  -- Extract function declarations
  let funs  = getFunDecls (moduleDecls modul)
      binds = map funToBind funs

  -- Generate uvars for each bind upfront so they can be typechecked in any order
  bindEnv <- mapM (\(Bind n _ _) -> (n, ) . Forall [] . TVar <$> fresh) binds
  let env'' = Map.fromList bindEnv <> env'

  -- Typecheck each bind
  (env''', binds') <- mapAccumLM (generateBind mempty) env'' binds

  -- Reconstruct module with typed declarations
  let datadecls = map T.DataDecl datas
      fundecls  = map (T.FunDecl . bindToFun) binds'
      moduleT   = T.Module { T.moduleName    = moduleName modul
                           , T.moduleImports = moduleImports modul
        -- At this point we should have expanded all exports into a flat list (see
        -- ExpandExports) so we can safely extract the fst of each.
                           , T.moduleExports = map fst (moduleExports modul)
                           , T.moduleDecls   = datadecls <> fundecls
                           }

  pure (env''', moduleT)

getFunDecls :: [Decl_ n e ty] -> [Fun_ n e ty]
getFunDecls = getDeclBy $ \case
  FunDecl f -> Just f
  _         -> Nothing

getDataDecls :: [Decl_ n e ty] -> [Data_ n]
getDataDecls = getDeclBy $ \case
  DataDecl d -> Just d
  _          -> Nothing

getDeclBy :: (Decl_ n e ty -> Maybe a) -> [Decl_ n e ty] -> [a]
getDeclBy _       []         = []
getDeclBy extract (d : rest) = case extract d of
  Just e  -> e : getDeclBy extract rest
  Nothing -> getDeclBy extract rest

funToBind :: Fun_ Name Can.Exp (Type_ Name) -> Bind
funToBind fun = Bind (funName fun) scheme equations
 where
  scheme    = tyToScheme <$> funType fun
  equations = map defToEquation (funDefs fun)

bindToFun :: BindT -> T.Fun
bindToFun (BindT name equations scheme) = T.Fun
  { T.funName = name
  , T.funType = scheme
  , T.funDefs = map equationToDef equations
  }

defToEquation :: Def_ Name Can.Exp -> ([Pattern_ Name], Exp)
defToEquation Def { defArgs = pats, defExpr = e } = (pats, fromSyn e)

equationToDef :: ([Pattern_ Name], ExpT) -> T.Def
equationToDef (pats, expr) = T.Def { T.defArgs = pats, T.defExpr = expr }

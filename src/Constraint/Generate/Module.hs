-- | Generate constraints for a whole Lam module
-- This basically involves generating constraints for each bind, accumulating an
-- environment as we go.
-- We will need an initial environment containing (possibly unknown) types for
-- any imported items.
--
-- = Typeclass generation
-- Typeclasses generate new names that can be used in axiom schemes, along
-- with a record type for the typeclass methods.
-- E.g.
-- >    class Eq a where
-- >      (==) : a -> a -> Bool
--
-- generates the following:
--
-- - a new axiom scheme name "Semigroup"
-- - a new function declaration:
--
-- > append : Semigroup a => a -> a -> a
--
-- TODO: how do we represent multiple parameters in a typeclass?
--
-- = Instance generation
-- Typeclass instances generate axiom schemes, which declare that a particular
-- type is a member of a particular typeclass. They also generate function
-- declarations which need to be namespaced somehow to ensure they don't clash
-- with any existing functions in the module.
-- E.g.
--
-- >    instance Eq a => Eq [a] where
-- >      (==) []       []       = True
-- >      (==) (x : xs) (y : ys) = x == y && xs == ys
-- >      (==) _        _        = False
--
-- This generates a new axiom scheme
--
-- > CForall [a] (Eq a) Eq [Lam.Primitive.List a]
--
-- and we also typecheck the methods in the instance, but leave dictionary
-- generation to the compiler.

module Constraint.Generate.Module where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

import           Constraint.Generate.M
import qualified Syn.Typed                     as T
import           Syn                     hiding ( Name
                                                , fn
                                                )
import qualified Canonical                     as Can
import           Canonical                      ( Name(..) )
import           Constraint
import           Constraint.Expr                ( Exp
                                                , ExpT
                                                )
import           Constraint.FromSyn             ( fromSyn
                                                , tyToScheme
                                                , tyToType
                                                )
import           Constraint.Generate.Bind
import qualified Constraint.Generate.Data
import           Util

-- The typeclasses we know about
-- typeclass name => names and types of the methods
type Typeclasses = Map Name TypeEnv

generateModule
  :: (Typeclasses, TypeEnv)
  -> Can.Module
  -> GenerateM ((Typeclasses, TypeEnv), T.Module)
generateModule (typeclasses, env) modul = do
  -- Extract data declarations
  let datas = map Constraint.Generate.Data.translate
                  (getDataDecls (moduleDecls modul))
      env' = env <> mconcat (map Constraint.Generate.Data.generate datas)

  -- For each typeclass, generate constrained types for the methods and add
  -- these to the environment. E.g.
  --
  -- > class Semigroup a where
  -- >         append : a -> a -> a
  --
  -- generates
  --
  -- > append : Semigroup a => a -> a -> a
  let moduleTypeclasses = Map.fromList $ map
        (\t -> (typeclassName t, generateMethods t))
        (getTypeclassDecls (moduleDecls modul))
      allMethods   = mconcat $ Map.elems moduleTypeclasses
      typeclasses' = moduleTypeclasses <> typeclasses

  -- Generate an axiom scheme from instance declarations
  let instances   = getInstanceDecls (moduleDecls modul)
      axiomScheme = map instanceToAxiom instances

  let typeclassNames' = Map.keys typeclasses'

  -- Extract function declarations
  let funs  = getFunDecls (moduleDecls modul)
      binds = map funToBind funs

  -- Check that every typeclass constraint is of a known typeclass
  let unknownTypeclassConstraints =
        let typeclassesInConstraints = concatMap
              (\(Bind _ msch _) ->
                maybe [] (\(Forall _ q _) -> constraintTypeclasses q) msch
              )
              binds
        in  filter (`notElem` typeclassNames') typeclassesInConstraints
  mapM_ (throwError . UnknownTypeclass) unknownTypeclassConstraints

  -- Generate uvars for each bind upfront so they can be typechecked in any order
  bindEnv <- mapM (\(Bind n _ _) -> (n, ) . Forall [] mempty . TVar <$> fresh)
                  binds
  let env'' = Map.fromList bindEnv <> env' <> allMethods

  -- Typecheck each bind
  (env''', binds') <- mapAccumLM (generateBind axiomScheme) env'' binds

  -- Typecheck each typeclass instance method
  instances'       <- mapM (generateInstance axiomScheme env''' typeclasses')
                           instances


  -- Reconstruct module with typed declarations
  let
    datadecls      = map T.DataDecl datas
    fundecls       = map (T.FunDecl . bindToFun) binds'
    instancedecls  = map T.TypeclassInst instances'
    typeclassdecls = map (T.TypeclassDecl . translateTypeclass)
      $ getTypeclassDecls (moduleDecls modul)
    moduleT = T.Module
      { T.moduleName    = moduleName modul
      , T.moduleImports = moduleImports modul
      -- At this point we should have expanded all exports into a flat list (see
      -- ExpandExports) so we can safely extract the fst of each.
      , T.moduleExports = map fst (moduleExports modul)
      , T.moduleDecls = datadecls <> fundecls <> instancedecls <> typeclassdecls
      }

  pure ((typeclasses', env'''), moduleT)

-- Given an environment, a set of typeclasses and a typeclass instance,
-- typecheck each of its methods
-- For each method:
-- - construct the correct type annotation by substituting the instance
--   types into the typeclass method signature
-- - typecheck the method with generateBind, throwing an error on failure
generateInstance
  :: AxiomScheme
  -> TypeEnv
  -> Typeclasses
  -> Instance_ Name Can.Exp
  -> GenerateM T.Instance
generateInstance axs env typeclasses inst =
  case Map.lookup (instanceName inst) typeclasses of
    Nothing               -> throwError (UnknownTypeclass (instanceName inst))
    Just typeclassMethods -> do
      -- The instanceName (i.e. the typeclass name) is canonical, so it will
      -- be scoped to the module where the typeclass is defined. The instance
      -- methods, however, will be scoped to the current module.
      -- To find the type scheme of the method as defined in the class, we need
      -- to construct a name in the scope of the module where the class is
      -- defined.
      let
        checkMethod (n@(Local _), _) =
          error $ "Unexpected local name " <> show n
        checkMethod (TopLevel thisModule name, defs) =
          let methodName =
                  let (TopLevel m _) = instanceName inst in TopLevel m name
          in
            case Map.lookup methodName typeclassMethods of
              Nothing -> throwError (UnknownInstanceMethod methodName)
              Just (Forall vars q t) ->
                let
                  -- substitute the instance types into the scheme
                  -- this may fail for multi-param typeclasses if the free type
                  -- variables are in the wong order - it's a bit hacky.
                  -- TODO: error if the number of vars doesn't match the number of
                  --       instanceTypes
                  subst = zip vars (map tyToType (instanceTypes inst))
                  sch'  = Forall [] (sub subst q) (sub subst t)
                  bind  = Bind (TopLevel thisModule name)
                               (Just sch')
                               (map defToEquation defs)
                in
                  do
                    (_, BindT _ typedDefs sch) <- generateBind axs env bind
                    let typedDefs' = map (uncurry T.Def) typedDefs
                    pure (TopLevel thisModule name, sch, typedDefs')
      methods <- mapM checkMethod (instanceDefs inst)
      pure T.Instance { T.instanceName  = instanceName inst
                      , T.instanceTypes = map tyToType (instanceTypes inst)
                      , T.instanceDefs  = methods
                      }

getFunDecls :: [Decl_ n e ty] -> [Fun_ n e ty]
getFunDecls = getDeclBy $ \case
  FunDecl f -> Just f
  _         -> Nothing

getDataDecls :: [Decl_ n e ty] -> [Data_ n]
getDataDecls = getDeclBy $ \case
  DataDecl d -> Just d
  _          -> Nothing

getTypeclassDecls :: [Decl_ n e ty] -> [Typeclass_ n]
getTypeclassDecls = getDeclBy $ \case
  TypeclassDecl t -> Just t
  _               -> Nothing

getInstanceDecls :: [Decl_ n e ty] -> [Instance_ n e]
getInstanceDecls = getDeclBy $ \case
  TypeclassInst i -> Just i
  _               -> Nothing

getDeclBy :: (Decl_ n e ty -> Maybe a) -> [Decl_ n e ty] -> [a]
getDeclBy _       []         = []
getDeclBy extract (d : rest) = case extract d of
  Just e  -> e : getDeclBy extract rest
  Nothing -> getDeclBy extract rest

translateTypeclass :: Can.Typeclass -> T.Typeclass
translateTypeclass t = T.Typeclass
  { T.typeclassName   = typeclassName t
  , T.typeclassTyVars = typeclassTyVars t
  , T.typeclassDefs   = mapSnd tyToType (typeclassDefs t)
  }

funToBind :: Fun_ Name Can.Exp (Type_ Name) -> Bind
funToBind fun = Bind (funName fun) scheme equations
 where
  scheme    = tyToScheme (funConstraint fun) <$> funType fun
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

-- We don't yet support instance constraints so the first constraint of
-- the axiom will always be empty.
instanceToAxiom :: Instance_ Name e -> Axiom
instanceToAxiom inst =
  let types = map tyToType (instanceTypes inst)
      vars  = ftv types
  in  AForall vars mempty (Inst (instanceName inst) types)

-- | Generate constrained types for all the methods in a given typeclass
generateMethods :: Typeclass_ Name -> TypeEnv
generateMethods tc = Map.fromList $ mapSnd constrain (typeclassDefs tc)
 where
  constrain ty =
    let vars = map R (typeclassTyVars tc)
    in  Forall vars (Inst (typeclassName tc) (map TVar vars)) (tyToType ty)

-- | Given a constraint, returns a list of the typeclasses it references
constraintTypeclasses :: Constraint.Constraint -> [Name]
constraintTypeclasses (a :^: b) =
  constraintTypeclasses a <> constraintTypeclasses b
constraintTypeclasses (Inst n _) = [n]
constraintTypeclasses _          = []

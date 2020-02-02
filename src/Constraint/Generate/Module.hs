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

import qualified Data.Map.Strict               as Map
import           Control.Monad                  ( void )

import           Constraint.Generate.M
import           Syn                     hiding ( Name
                                                , fn
                                                )
import           Canonical                      ( Name(..) )
import           Constraint
import           Constraint.Expr                ( Exp
                                                , ExpT
                                                , Scheme(..)
                                                )
import           Constraint.FromSyn             ( fromSyn
                                                , tyToScheme
                                                , tyToType
                                                )
import           Constraint.Generate.Bind
import           Util

generateModule
  :: Env
  -> Module_ Name (Syn_ Name) (Type_ Name)
  -> GenerateM (Env, Module_ Name ExpT Scheme)
generateModule env modul = do
  -- Extract data declarations
  let datas       = getDataDecls (moduleDecls modul)
  let env'        = foldl generateDataDecl env datas

  -- For each typeclass, generate constrained types for the methods and add
  -- these to the environment. E.g.
  --
  -- > class Semigroup a where
  -- >         append : a -> a -> a
  --
  -- generates
  --
  -- > append : Semigroup a => a -> a -> a
  let typeclasses = getTypeclassDecls (moduleDecls modul)
  let typeclassMethods =
        map (\t -> (typeclassName t, generateMethods t)) typeclasses
  let allMethods  = Map.fromList (concatMap snd typeclassMethods)

  -- Generate an axiom scheme from instance declarations
  let instances   = getInstanceDecls (moduleDecls modul)
  let axiomScheme = map instanceToAxiom instances

  -- Check that every instance is of a known typeclass
  let unknownTypeclassInstances =
        let typeclassNames = map typeclassName typeclasses
        in  filter (\i -> instanceName i `notElem` typeclassNames) instances
  mapM_ (throwError . UnknownTypeclass . instanceName) unknownTypeclassInstances

  -- TODO: typecheck instance methods
  --       fetch and include imported typeclasses and instances

  -- Extract function declarations
  let funs  = getFunDecls (moduleDecls modul)
  let binds = map funToBind funs

  -- Check that every typeclass constraint is of a known typeclass
  let unknownTypeclassConstraints =
        let typeclassNames           = map typeclassName typeclasses
            typeclassesInConstraints = concatMap
              (\(Bind _ msch _) ->
                maybe [] (\(Forall _ q _) -> constraintTypeclasses q) msch
              )
              binds
        in  filter (`notElem` typeclassNames) typeclassesInConstraints
  mapM_ (throwError . UnknownTypeclass) unknownTypeclassConstraints

  -- Generate uvars for each bind upfront so they can be typechecked in any order
  bindEnv <- mapM (\(Bind n _ _) -> (n, ) . Forall [] mempty . TVar <$> fresh)
                  binds
  let env'' = Map.fromList bindEnv <> env' <> allMethods

  -- Typecheck each bind
  (env''', binds') <- mapAccumLM (generateBind axiomScheme) env'' binds

  -- Typecheck each typeclass instance method
  mapM_ (generateInstance axiomScheme env''' typeclassMethods) instances

  -- Reconstruct module with typed declarations
  let datadecls = map DataDecl datas
      fundecls  = map (FunDecl . bindToFun) binds'

  pure (env''', modul { moduleDecls = datadecls <> fundecls })

-- Given an environment, a set of typeclasses and a typeclass instance,
-- typecheck each of its methods
-- For each method:
-- - construct the correct type annotation by substituting the instance
--   types into the typeclass method signature
-- - typecheck the method with generateBind, throwing an error on failure
generateInstance
  :: AxiomScheme
  -> Env
  -> [(Name, [(Name, Scheme)])]
  -> Instance_ Name (Syn_ Name)
  -> GenerateM ()
generateInstance axs env typeclasses inst =
  case lookup (instanceName inst) typeclasses of
    Nothing               -> throwError (UnknownTypeclass (instanceName inst))
    Just typeclassMethods -> do
      let checkMethod (name, defs) = case lookup name typeclassMethods of
            Nothing -> throwError (UnknownInstanceMethod name)
            Just (Forall vars q t) ->
              let
                -- substitute the instance types into the scheme
                -- this may fail for multi-param typeclasses if the free type
                -- variables are in the wong order - it's a bit hacky.
                -- TODO: error if the number of vars doesn't match the number of
                --       instanceTypes
                  subst = zip vars (map tyToType (instanceTypes inst))
                  sch'  = Forall [] (sub subst q) (sub subst t)
                  bind  = Bind name (Just sch') (map defToEquation defs)
              in  void (generateBind axs env bind)
      mapM_ checkMethod (instanceDefs inst)

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

funToBind :: Fun_ Name (Syn_ Name) (Type_ Name) -> Bind
funToBind fun = Bind (funName fun) (Just scheme) equations
 where
  scheme    = tyToScheme (funConstraint fun) (funType fun)
  equations = map defToEquation (funDefs fun)

bindToFun :: BindT -> Fun_ Name ExpT Scheme
bindToFun (BindT name equations scheme) = Fun
  { funComments   = []
  , funName       = name
  , funType       = scheme
  , funConstraint = Nothing
  , funDefs       = map equationToDef equations
  }

defToEquation :: Def_ Name (Syn_ Name) -> ([Pattern_ Name], Exp)
defToEquation Def { defArgs = pats, defExpr = e } = (pats, fromSyn e)

equationToDef :: ([Pattern_ Name], ExpT) -> Def_ Name ExpT
equationToDef (pats, expr) = Def { defArgs = pats, defExpr = expr }

-- Generate new bindings for data declarations.
--
--           data Maybe a = Just a | Nothing
-- generates
--           Just : Forall a. a -> Maybe a
--           Nothing : Forall a.
--
--           data User = User { name : String, age : Int }
-- generates
--           User : String -> Int -> User
--           name : User -> String
--           age  : User -> Int
--
-- TODO: generate record field selectors
generateDataDecl :: Env -> Data_ Name -> Env
generateDataDecl env d =
  let tyvars = map (R . Local) (dataTyVars d)
      tycon  = TCon (dataName d) (map TVar tyvars)
      mkType args = Forall tyvars mempty (foldr (fn . tyToType) tycon args)
      mkCon (DataCon   name args  ) = (name, mkType args)
      mkCon (RecordCon name fields) = (name, mkType (map snd fields))
  in  env <> Map.fromList (map mkCon (dataCons d))

-- We don't yet support instance constraints so the first constraint of
-- the axiom will always be empty.
instanceToAxiom :: Instance_ Name e -> Axiom
instanceToAxiom inst =
  let types = map tyToType (instanceTypes inst)
      vars  = ftv types
  in  AForall vars mempty (Inst (instanceName inst) types)

-- | Generate constrained types for all the methods in a given typeclass
generateMethods :: Typeclass_ Name -> [(Name, Scheme)]
generateMethods tc = mapSnd constrain (typeclassDefs tc)
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

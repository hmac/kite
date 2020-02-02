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
  let datas = getDataDecls (moduleDecls modul)
  let env'  = foldl generateDataDecl env datas

  -- TODO: typeclass declarations
  --       instance declarations

  -- Extract function declarations
  let funs  = getFunDecls (moduleDecls modul)
  let binds = map funToBind funs

  -- Generate uvars for each bind upfront so they can be typechecked in any order
  bindEnv <- mapM (\(Bind n _ _) -> (n, ) . Forall [] mempty . TVar <$> fresh)
                  binds
  let env'' = Map.fromList bindEnv <> env'

  -- Typecheck each bind
  (env''', binds') <- mapAccumLM generateBind env'' binds

  -- Reconstruct module with typed declarations
  let datadecls = map DataDecl datas
      fundecls  = map (FunDecl . bindToFun) binds'

  pure (env''', modul { moduleDecls = datadecls <> fundecls })

getFunDecls :: [Decl_ n e ty] -> [Fun_ n e ty]
getFunDecls (FunDecl f : rest) = f : getFunDecls rest
getFunDecls (_         : rest) = getFunDecls rest
getFunDecls []                 = []

getDataDecls :: [Decl_ n e ty] -> [Data_ n]
getDataDecls (DataDecl d : rest) = d : getDataDecls rest
getDataDecls (_          : rest) = getDataDecls rest
getDataDecls []                  = []

-- TODO: typeclass constraints
funToBind :: Fun_ Name (Syn_ Name) (Type_ Name) -> Bind
funToBind fun = Bind (funName fun) (Just scheme) equations
 where
  scheme    = tyToScheme (funType fun)
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

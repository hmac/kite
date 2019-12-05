module Infer where

-- TODO: delete this module

import           Data.List                      ( nub )
import           Infer.W
import           Syntax
import qualified Data.HashMap.Strict           as Map

buildModEnv :: Module Syn -> Env
buildModEnv m = Map.fromList $ concatMap extractSig (moduleDecls m)
 where
  extractSig :: Decl Syn -> [(Name, Scheme)]
  extractSig (FunDecl  f) = [(funName f, mkScheme (funType f))]
  extractSig (DataDecl d) = extractDataSig d
  -- TODO: the other decls

-- Extract type signatures from a data declaration
-- e.g. this type
-- data Maybe a = Just a | Nothing
-- results in these signatures:
-- Just :    ∀ a. a -> Maybe a     Forall [a] (TyArr a (TyApp Maybe [a]))
-- Nothing : ∀ a.      Maybe a     Forall [a] (TyApp Maybe [a])
extractDataSig :: Data -> [(Name, Scheme)]
extractDataSig Data { dataName = name, dataTyVars = vars, dataCons = cons } =
  map extract cons
 where
  extract DataCon { conName = cName, conArgs = cArgs } =
    (cName, Forall vars (foldr TyArr (TyApp name (map TyVar vars)) cArgs))

mkScheme :: Ty -> Scheme
mkScheme TyInt      = Forall [] TyInt
mkScheme TyFloat    = Forall [] TyFloat
mkScheme TyString   = Forall [] TyString
mkScheme (TyHole _) = undefined -- not sure what to do here
mkScheme (TyVar  a) = Forall [a] (TyVar a)
mkScheme (TyApp n ts) =
  let vars = concatMap (\t -> let (Forall as _) = mkScheme t in as) ts
  in  Forall vars (TyApp n ts)
mkScheme (TyArr a b) =
  let (Forall as _) = mkScheme a
      (Forall bs _) = mkScheme b
  in  Forall (nub (as ++ bs)) (TyArr a b)
mkScheme (TyList a) = let (Forall as _) = mkScheme a in Forall as (TyList a)
mkScheme (TyTuple as) =
  let vars = concatMap (\t -> let (Forall xs _) = mkScheme t in xs) as
  in  Forall (nub vars) (TyTuple as)

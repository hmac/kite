module Canonicalise where

import qualified Data.Map.Strict               as Map

import           Util
import           Data.Name
import           Syn
import           Canonical                      ( Name(..) )
import qualified Canonical                     as Can
import qualified Canonical.Primitive

-- Converts raw names to canonical names.

-- To canonicalise a module we need to know what names are imported from other
-- modules. We represent that as map from name to module.

type Imports = Map.Map Syn.Name Syn.ModuleName
type Env = (ModuleName, Imports)

buildImports :: Syn.Module Syn.Syn -> Imports
buildImports m =
  let imports = concatMap (\imp -> fmap (, importName imp) (importItems imp))
                          (moduleImports m)
  in  Map.fromList (imports <> Canonical.Primitive.primitives)

canonicaliseModule :: Syn.Module Syn.Syn -> Can.Module Can.Exp
canonicaliseModule m =
  let imports = buildImports m
      env     = (moduleName m, imports)
  in  m { moduleExports = fmap Can.Local (moduleExports m)
        , moduleDecls   = fmap (canonicaliseDecl env) (moduleDecls m)
        }

canonicaliseDecl :: Env -> Syn.Decl Syn.Syn -> Can.Decl Can.Exp
canonicaliseDecl env = \case
  FunDecl       f -> FunDecl $ canonicaliseFun env f
  DataDecl      d -> DataDecl $ canonicaliseData env d
  TypeclassDecl t -> TypeclassDecl $ canonicaliseTypeclass env t
  TypeclassInst i -> TypeclassInst $ canonicaliseInstance env i
  Comment       s -> Comment s

canonicaliseFun :: Env -> Syn.Fun Syn.Syn -> Can.Fun Can.Exp
canonicaliseFun (mod, imps) f = f
  { funName       = TopLevel mod (funName f)
  , funDefs       = fmap (canonicaliseDef (mod, imps)) (funDefs f)
  , funConstraint = canonicaliseConstraint (mod, imps) <$> funConstraint f
  , funType       = canonicaliseType (mod, imps) <$> funType f
  }

canonicaliseType :: Env -> Syn.Type -> Can.Type
canonicaliseType env = \case
  -- The type names String and Int are reserved, so they always refer to the
  -- corresponding primitive types.
  -- TODO: this is a bit of a hack and we do different things elsewhere. We
  -- should standardise on how to deal with primitives.
  TyCon "String" _  -> TyString
  TyCon "Int"    _  -> TyInt
  -- Note: type variables are assumed to be local to the type
  -- this may need rethinking when we support type aliases
  TyCon n ts -> TyCon (canonicaliseName env n) (map (canonicaliseType env) ts)
  TyVar   v         -> TyVar (Local v)
  TyList  a         -> TyList (canonicaliseType env a)
  TyTuple as        -> TyTuple $ fmap (canonicaliseType env) as
  TyHole  n         -> TyHole n
  TyInt             -> TyInt
  TyString          -> TyString
  TyFun a b         -> TyFun (canonicaliseType env a) (canonicaliseType env b)

canonicaliseConstraint :: Env -> Syn.Constraint -> Can.Constraint
canonicaliseConstraint env = \case
  CInst n tys ->
    let n' = canonicaliseName env n
    in  CInst n' $ fmap (canonicaliseType env) tys
  CTuple a b ->
    CTuple (canonicaliseConstraint env a) (canonicaliseConstraint env b)

canonicaliseData :: Env -> Syn.Data -> Can.Data
canonicaliseData (mod, imps) d = d
  { dataName = TopLevel mod (dataName d)
  , dataCons = fmap (canonicaliseDataCon (mod, imps)) (dataCons d)
  }

canonicaliseDataCon :: Env -> Syn.DataCon -> Can.DataCon
canonicaliseDataCon (mod, imps) DataCon { conName = name, conArgs = args } =
  DataCon { conName = TopLevel mod name
          , conArgs = fmap (canonicaliseType (mod, imps)) args
          }
canonicaliseDataCon (mod, imps) RecordCon { conName = name, conFields = fields }
  = RecordCon
    { conName   = TopLevel mod name
    , conFields = map (bimap (TopLevel mod) (canonicaliseType (mod, imps)))
                      fields
    }

canonicaliseTypeclass :: Env -> Syn.Typeclass -> Can.Typeclass
canonicaliseTypeclass (mod, imps) t = t
  { typeclassName   = TopLevel mod (typeclassName t)
  , typeclassDefs   = bimapL (TopLevel mod)
                             (canonicaliseType (mod, imps))
                             (typeclassDefs t)
  , typeclassTyVars = map Local (typeclassTyVars t)
  }

canonicaliseInstance :: Env -> Syn.Instance Syn.Syn -> Can.Instance Can.Exp
canonicaliseInstance env@(mod, _) i = i
  { instanceName  = canonicaliseName env (instanceName i)
  , instanceDefs  = fmap (bimap (TopLevel mod) (fmap (canonicaliseDef env)))
                         (instanceDefs i)
  , instanceTypes = fmap (canonicaliseType env) (instanceTypes i)
  }

canonicaliseDef :: Env -> Syn.Def Syn.Syn -> Can.Def Can.Exp
canonicaliseDef env d =
  let res    = map (canonicalisePattern env) (defArgs d)
      locals = concatMap fst res
      args   = map snd res
  in  d { defExpr = canonicaliseExp env locals (defExpr d), defArgs = args }

canonicaliseExp :: Env -> [Syn.Name] -> Syn.Syn -> Can.Exp
canonicaliseExp env = go
 where
  go locals = \case
    Var n | n `elem` locals -> Var (Local n)
          | otherwise       -> Var $ canonicaliseName env n
    Con n | n `elem` locals -> Con (Local n)
          | otherwise       -> Con $ canonicaliseName env n
    Abs  ns    e        -> Abs (fmap Local ns) $ go (ns <> locals) e
    App  a     b        -> App (go locals a) (go locals b)
    Let  binds e        -> canonicaliseLet (binds, e)
    Case e     alts     -> canonicaliseCase (e, alts)
    TupleLit es         -> TupleLit $ fmap (go locals) es
    ListLit  es         -> ListLit $ fmap (go locals) es
    StringLit pre parts -> StringLit pre $ mapFst (go locals) parts
    Hole   n            -> Hole (canonicaliseName env n)
    IntLit i            -> IntLit i
   where
    canonicaliseLet :: ([(Syn.Name, Syn.Syn)], Syn.Syn) -> Can.Exp
    canonicaliseLet (binds, e) =
      let (locals', binds') = foldl
            (\(locals, acc) (n, e) ->
              let b = (Local n, go locals e) in (n : locals, b : acc)
            )
            (locals, [])
            binds
      in  Let binds' (go locals' e)

    canonicaliseCase :: (Syn.Syn, [(Pattern, Syn.Syn)]) -> Can.Exp
    canonicaliseCase (e, alts) =
      let alts' = map
            (\(pat, e) ->
              let (vars, pat') = canonicalisePattern env pat
                  e'           = go (vars <> locals) e
              in  (pat', e')
            )
            alts
      in  Case (go locals e) alts'

canonicalisePattern :: Env -> Syn.Pattern -> ([Syn.Name], Can.Pattern)
canonicalisePattern env = \case
  VarPat n -> ([n], VarPat (Local n))
  WildPat  -> ([], WildPat)
  IntPat i -> ([], IntPat i)
  TuplePat pats ->
    let res   = map (canonicalisePattern env) pats
        vars  = concatMap fst res
        pats' = map snd res
    in  (vars, TuplePat pats')
  ListPat pats ->
    let res   = map (canonicalisePattern env) pats
        vars  = concatMap fst res
        pats' = map snd res
    in  (vars, ListPat pats')
  ConsPat c pats ->
    let c'    = canonicaliseName env c
        res   = map (canonicalisePattern env) pats
        vars  = concatMap fst res
        pats' = map snd res
    in  (vars, ConsPat c' pats')

canonicaliseName :: Env -> Syn.Name -> Can.Name
canonicaliseName (thisModule, imps) n = case Map.lookup n imps of
  Just i  -> TopLevel i n
  Nothing -> TopLevel thisModule n

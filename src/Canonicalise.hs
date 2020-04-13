module Canonicalise
  ( canonicaliseModule
  , Error(..)
  )
where

import qualified Data.Map.Strict               as Map

import           Prelude                 hiding ( mod )
import           Util
import           Data.Name
import           Syn
import           Canonical                      ( Name(..) )
import qualified Canonical                     as Can
import qualified Canonical.Primitive

data Error = UnknownVar RawName deriving (Show, Eq)

-- Converts raw names to canonical names.

-- To canonicalise a module we need to know what names are imported from other
-- modules. We represent that as map from name to module.
-- We also need to know what names are defined in the module. We represent that
-- as a list of RawName.

type Imports = Map.Map RawName Syn.ModuleName
type Env = (ModuleName, Imports, [RawName])

buildImports :: Syn.Module Syn.Syn -> Imports
buildImports m =
  let imports =
          [ (subitem, importName imp)
          | imp     <- moduleImports m
          , item    <- importItems imp
          , subitem <- flattenImportItem m item
          ]
  in  Map.fromList (imports <> Canonical.Primitive.primitives)

-- Given an ImportItem which may contain nested names (e.g. Either(Left, Right))
-- flatten it to a list of names: [Either, Left, Right]
-- TODO: at what point do we check that the import names actually exist in the
-- imported module?
flattenImportItem :: Syn.Module Syn.Syn -> Syn.ImportItem -> [RawName]
flattenImportItem modul = \case
  ImportSingle { importItemName = n } -> [n]
  ImportAll { importItemName = n } -> n : constructorsOrMethodsForType modul n
  ImportSome { importItemName = n, importItemConstructors = cs } -> n : cs

-- Given a name of a typeclass or a data type, find all the names that would be
-- imported by a (..) style import.
-- For data types, this is all the constructor names.
-- For typeclasses, it's all the method names.
constructorsOrMethodsForType :: Syn.Module Syn.Syn -> RawName -> [RawName]
constructorsOrMethodsForType modul tyname =
  let datas = dataDecls modul
  in  case find ((== tyname) . dataName) datas of
        Just d -> map conName (dataCons d)
        Nothing ->
          case find ((== tyname) . typeclassName) (typeclassDecls modul) of
            Just t  -> map fst (typeclassDefs t)
            Nothing -> []

-- TODO: some trivial fusion to be done here
topLevelNamesForModule :: Syn.Module Syn.Syn -> [RawName]
topLevelNamesForModule m =
  typeclassNames <> methodNames <> dataNames <> constructorNames <> funNames
 where
  typeclassNames   = map typeclassName (typeclassDecls m)
  methodNames      = concatMap (map fst . typeclassDefs) (typeclassDecls m)
  dataNames        = map dataName (dataDecls m)
  constructorNames = concatMap (map conName . dataCons) (dataDecls m)
  funNames         = map funName (funDecls m)

canonicaliseModule :: Syn.Module Syn.Syn -> Either Error Can.Module
canonicaliseModule m =
  let imports       = buildImports m
      topLevelNames = topLevelNamesForModule m
      env           = (moduleName m, imports, topLevelNames)
      exports =
          [ (Can.Local export, map Can.Local subexports)
          | (export, subexports) <- moduleExports m
          ]
  in  do
        decls <- mapM (canonicaliseDecl env) (moduleDecls m)
        pure m { moduleExports = exports, moduleDecls = decls }

canonicaliseDecl :: Env -> Syn.Decl Syn.Syn -> Either Error (Can.Decl Can.Exp)
canonicaliseDecl env = \case
  FunDecl       f -> FunDecl <$> canonicaliseFun env f
  DataDecl      d -> DataDecl <$> canonicaliseData env d
  TypeclassDecl t -> TypeclassDecl <$> canonicaliseTypeclass env t
  TypeclassInst i -> TypeclassInst <$> canonicaliseInstance env i
  Comment       s -> pure (Comment s)

-- Functions in a where clause are visible (only) to the parent function and to
-- other functions in the clause.
canonicaliseFun :: Env -> Syn.Fun Syn.Syn -> Either Error (Can.Fun Can.Exp)
canonicaliseFun env@(modul, imps, globals) f = do
  let whereNames = map whereName (funWhere f)
      globals'   = whereNames <> globals
      env'       = (modul, imps, globals')
  defs       <- mapM (canonicaliseDef env') (funDefs f)
  constraint <- traverse (canonicaliseConstraint env) (funConstraint f)
  type_      <- traverse (canonicaliseType env) (funType f)
  where_     <- mapM (canonicaliseWhere env') (funWhere f)
  pure f { funName       = TopLevel modul (funName f)
         , funDefs       = defs
         , funConstraint = constraint
         , funType       = type_
         , funWhere      = where_
         }

canonicaliseWhere
  :: Env -> Syn.Where Syn.Syn -> Either Error (Can.Where Can.Exp)
canonicaliseWhere env@(modul, _, _) w = do
  defs       <- mapM (canonicaliseDef env) (whereDefs w)
  constraint <- traverse (canonicaliseConstraint env) (whereConstraint w)
  type_      <- traverse (canonicaliseType env) (whereType w)
  pure w { whereName       = TopLevel modul (whereName w)
         , whereDefs       = defs
         , whereConstraint = constraint
         , whereType       = type_
         }

canonicaliseType :: Env -> Syn.Type -> Either Error Can.Type
canonicaliseType env = \case
  -- The type names String and Int are reserved, so they always refer to the
  -- corresponding primitive types.
  -- TODO: this is a bit of a hack and we do different things elsewhere. We
  -- should standardise on how to deal with primitives.
  TyCon "String" _ -> pure TyString
  TyCon "Int"    _ -> pure TyInt
  -- Note: type variables are assumed to be local to the type
  -- this may need rethinking when we support type aliases
  TyCon n ts ->
    TyCon <$> canonicaliseName env n <*> mapM (canonicaliseType env) ts
  TyVar   v  -> pure $ TyVar (Local v)
  TyList  a  -> TyList <$> canonicaliseType env a
  TyTuple as -> TyTuple <$> mapM (canonicaliseType env) as
  TyHole  n  -> pure $ TyHole n
  TyInt      -> pure TyInt
  TyString   -> pure TyString
  TyFun a b  -> TyFun <$> canonicaliseType env a <*> canonicaliseType env b

canonicaliseScheme :: Env -> Syn.Scheme -> Either Error Can.Scheme
canonicaliseScheme env (Forall vars c t) =
  Forall (map Local vars)
    <$> canonicaliseConstraint env c
    <*> canonicaliseType env t

canonicaliseConstraint :: Env -> Syn.Constraint -> Either Error Can.Constraint
canonicaliseConstraint env = \case
  CInst n tys ->
    CInst <$> canonicaliseName env n <*> mapM (canonicaliseType env) tys
  CTuple a b ->
    CTuple <$> canonicaliseConstraint env a <*> canonicaliseConstraint env b
  CNil -> pure CNil

canonicaliseData :: Env -> Syn.Data -> Either Error Can.Data
canonicaliseData env@(mod, _, _) d = do
  cons <- mapM (canonicaliseDataCon env) (dataCons d)
  pure $ d { dataName = TopLevel mod (dataName d), dataCons = cons }

canonicaliseDataCon :: Env -> Syn.DataCon -> Either Error Can.DataCon
canonicaliseDataCon env@(mod, _, _) DataCon { conName = name, conArgs = args }
  = DataCon (TopLevel mod name) <$> mapM (canonicaliseType env) args
canonicaliseDataCon env@(mod, _, _) RecordCon { conName = name, conFields = fields }
  = do
    fields' <- mapM
      (\(n, ty) -> (TopLevel mod n, ) <$> canonicaliseType env ty)
      fields
    pure RecordCon { conName = TopLevel mod name, conFields = fields' }

canonicaliseTypeclass :: Env -> Syn.Typeclass -> Either Error Can.Typeclass
canonicaliseTypeclass (mod, imps, locals) t = do
  -- Make any local type variables introduced by the typeclass visible to the
  -- types of the typeclass defs.
  let locals' = locals <> typeclassTyVars t
      env'    = (mod, imps, locals')
  defs <- mapM (\(n, ty) -> (TopLevel mod n, ) <$> canonicaliseType env' ty)
               (typeclassDefs t)
  pure t { typeclassName   = TopLevel mod (typeclassName t)
         , typeclassDefs   = defs
         , typeclassTyVars = map Local (typeclassTyVars t)
         }

canonicaliseInstance
  :: Env -> Syn.Instance Syn.Syn -> Either Error (Can.Instance Can.Exp)
canonicaliseInstance env@(mod, _, _) i = do
  defs <- mapM
    (\(n, ds) -> (TopLevel mod n, ) <$> mapM (canonicaliseDef env) ds)
    (instanceDefs i)
  types <- mapM (canonicaliseType env) (instanceTypes i)
  name  <- canonicaliseName env (instanceName i)
  pure i { instanceName = name, instanceDefs = defs, instanceTypes = types }

canonicaliseDef :: Env -> Syn.Def Syn.Syn -> Either Error (Can.Def Can.Exp)
canonicaliseDef env d = do
  trace (pShow env) (pure ())
  trace (pShow d)   (pure ())
  res <- mapM (canonicalisePattern env) (defArgs d)
  let locals = concatMap fst res
      args   = map snd res
  name <- canonicaliseName env (defName d)
  expr <- canonicaliseExp env locals (defExpr d)
  pure d { defName = name, defExpr = expr, defArgs = args }

canonicaliseExp :: Env -> [RawName] -> Syn.Syn -> Either Error Can.Exp
canonicaliseExp env = go
 where
  go :: [RawName] -> Syn.Syn -> Either Error Can.Exp
  go locals = \case
    Var n | n `elem` locals -> pure $ Var (Local n)
          | otherwise       -> Var <$> canonicaliseName env n
    Con n | n `elem` locals -> pure $ Con (Local n)
          | otherwise       -> Con <$> canonicaliseName env n
    Abs ns e        -> Abs (fmap Local ns) <$> go (ns <> locals) e
    App a  b        -> App <$> go locals a <*> go locals b
    LetA n ty a e   -> canonicaliseLetA locals n ty a e
    Let  binds e    -> canonicaliseLet locals (binds, e)
    Case e     alts -> canonicaliseCase locals (e, alts)
    TupleLit es     -> TupleLit <$> mapM (go locals) es
    ListLit  es     -> ListLit <$> mapM (go locals) es
    StringLit pre parts ->
      StringLit pre <$> mapM (\(e, s) -> (, s) <$> go locals e) parts
    Hole   n -> Hole <$> canonicaliseName env n
    IntLit i -> pure $ IntLit i
  -- Note: to handle recursive lets, in canonicaliseLet we would extend the set
  -- of locals with the variable name before canonicalising the bound
  -- expression. We currently don't do that because Lam's evaluation strategy is
  -- strict and the naive evaluation of recursive lets doesn't terminate. I'm
  -- not yet sure if we should support recursive lets at all - maybe you should
  -- always write a separate function if you want recursion?
  canonicaliseLet
    :: [RawName] -> ([(RawName, Syn.Syn)], Syn.Syn) -> Either Error Can.Exp
  canonicaliseLet locals (binds, e) = do
    (locals', binds') <- foldlM
      (\(locals'', acc) (varName, expr) -> do
        b <- (Local varName, ) <$> go locals'' expr
        pure (varName : locals'', b : acc)
      )
      (locals, [])
      binds
    Let binds' <$> go locals' e
  canonicaliseLetA
    :: [RawName]
    -> RawName
    -> Syn.Scheme
    -> Syn.Syn
    -> Syn.Syn
    -> Either Error Can.Exp
  canonicaliseLetA locals name sch expr body = do
    sch'  <- canonicaliseScheme env sch
    expr' <- go locals expr
    body' <- go (name : locals) body
    pure $ LetA (Local name) sch' expr' body'

  canonicaliseCase
    :: [RawName] -> (Syn.Syn, [(Pattern, Syn.Syn)]) -> Either Error Can.Exp
  canonicaliseCase locals (expr, alts) = do
    alts' <- mapM
      (\(pat, e) -> do
        (vars, pat') <- canonicalisePattern env pat
        e'           <- go (vars <> locals) e
        pure (pat', e')
      )
      alts
    expr' <- go locals expr
    pure $ Case expr' alts'

canonicalisePattern
  :: Env -> Syn.Pattern -> Either Error ([RawName], Can.Pattern)
canonicalisePattern env = \case
  VarPat n      -> pure ([n], VarPat (Local n))
  WildPat       -> pure ([], WildPat)
  IntPat   i    -> pure ([], IntPat i)
  TuplePat pats -> do
    res <- mapM (canonicalisePattern env) pats
    let vars  = concatMap fst res
        pats' = map snd res
    pure (vars, TuplePat pats')
  ListPat pats -> do
    res <- mapM (canonicalisePattern env) pats
    let vars  = concatMap fst res
        pats' = map snd res
    pure (vars, ListPat pats')
  ConsPat c pats -> do
    c'  <- canonicaliseName env c
    res <- mapM (canonicalisePattern env) pats
    let vars  = concatMap fst res
        pats' = map snd res
    pure (vars, ConsPat c' pats')
  StringPat s -> pure ([], StringPat s)

canonicaliseName :: Env -> RawName -> Either Error Can.Name
canonicaliseName (thisModule, imps, globals) n = case Map.lookup n imps of
  Just i  -> pure $ TopLevel i n
  Nothing -> if n `elem` globals
    then pure $ TopLevel thisModule n
    else Left (UnknownVar n)

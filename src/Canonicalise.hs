module Canonicalise where

import           Prelude                 hiding ( mod )

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

{-
  Note: Record labels
  -------------------

  Record labels are expected to be globally unique, so we don't qualify them
  with a module. Instead, which is a bit of a hack, we make them all Local.
  This means that two labels which are the same on the surface will be
  recognised as the same by the typechecker.

  Ideally we'd have a special namespace for record labels, so it's less
  confusing.

-}

type Imports = Map.Map RawName Syn.ModuleName
type Env = (ModuleName, Imports)

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
  ImportAll { importItemName = n } -> n : constructorsForType modul n
  ImportSome { importItemName = n, importItemConstructors = cs } -> n : cs

-- Given a name of a data type, find all the names that would be
-- imported by a (..) style import.
-- For data types, this is all the constructor names.
constructorsForType :: Syn.Module Syn.Syn -> RawName -> [RawName]
constructorsForType modul tyname =
  let datas = dataDecls modul
  in  case find ((== tyname) . dataName) datas of
        Just d  -> map conName (dataCons d)
        Nothing -> []

canonicaliseModule :: Syn.Module Syn.Syn -> Can.Module
canonicaliseModule m =
  let imports = buildImports m
      env     = (moduleName m, imports)
      exports =
          [ (Can.Local export, map Can.Local subexports)
          | (export, subexports) <- moduleExports m
          ]
  in  m { moduleExports = exports
        , moduleDecls   = map (canonicaliseDecl env) (moduleDecls m)
        }

canonicaliseDecl :: Env -> Syn.Decl Syn.Syn -> Can.Decl Can.Exp
canonicaliseDecl env = \case
  FunDecl  f -> FunDecl $ canonicaliseFun env f
  DataDecl d -> DataDecl $ canonicaliseData env d
  Comment  s -> Comment s

canonicaliseFun :: Env -> Syn.Fun Syn.Syn -> Can.Fun Can.Exp
canonicaliseFun (mod, imps) f = f
  { funName = TopLevel mod (funName f)
  , funDefs = fmap (canonicaliseDef (mod, imps)) (funDefs f)
  , funType = canonicaliseType (mod, imps) <$> funType f
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
  TyRecord fields -> TyRecord (map (bimap Local (canonicaliseType env)) fields)

canonicaliseScheme :: Env -> Syn.Scheme -> Can.Scheme
canonicaliseScheme _env _ = error "canonicaliseScheme: not implemented yet"

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

canonicaliseDef :: Env -> Syn.Def Syn.Syn -> Can.Def Can.Exp
canonicaliseDef env d =
  let res    = map (canonicalisePattern env) (defArgs d)
      locals = concatMap fst res
      args   = map snd res
  in  d { defExpr = canonicaliseExp env locals (defExpr d), defArgs = args }

canonicaliseExp :: Env -> [RawName] -> Syn.Syn -> Can.Exp
canonicaliseExp env = go
 where
  go locals = \case
    Var n | n `elem` locals -> Var (Local n)
          | otherwise       -> Var $ canonicaliseName env n
    Con n | n `elem` locals -> Con (Local n)
          | otherwise       -> Con $ canonicaliseName env n
    Abs ns    e       -> Abs (fmap Local ns) $ go (ns <> locals) e
    App a     b       -> App (go locals a) (go locals b)
    Let binds e       -> canonicaliseLet (binds, e)
    LetA n sch e body -> LetA (canonicaliseName env n)
                              (canonicaliseScheme env sch)
                              (go locals e)
                              (go (n : locals) body)
    Case e alts         -> canonicaliseCase (e, alts)
    TupleLit es         -> TupleLit $ fmap (go locals) es
    ListLit  es         -> ListLit $ fmap (go locals) es
    StringLit pre parts -> StringLit pre $ mapFst (go locals) parts
    Hole   n            -> Hole (canonicaliseName env n)
    IntLit i            -> IntLit i
    Record fields       -> Record $ bimapL Local (go locals) fields
    Project r l         -> Project (go locals r) (Local l)
   where
    canonicaliseLet :: ([(RawName, Syn.Syn)], Syn.Syn) -> Can.Exp
    canonicaliseLet (binds, e) =
      let (locals', binds') = foldl
            (\(locals_, acc) (varName, expr) ->
              -- Note: to handle recursive lets, here we would extend the set of
              -- locals with the variable name before canonicalising the bound
              -- expression. We currently don't do that because Lam's evaluation
              -- strategy is strict and the naive evaluation of recursive lets
              -- doesn't terminate. I'm not yet sure if we should support
              -- recursive lets at all - maybe you should always write a
              -- separate function if you want recursion?
              let b = (Local varName, go locals_ expr)
              in  (varName : locals_, b : acc)
            )
            (locals, [])
            binds
      in  Let binds' (go locals' e)

    canonicaliseCase :: (Syn.Syn, [(Pattern, Syn.Syn)]) -> Can.Exp
    canonicaliseCase (e, alts) =
      let alts' = map
            (\(pat, e_) ->
              let (vars, pat') = canonicalisePattern env pat
                  e'           = go (vars <> locals) e_
              in  (pat', e')
            )
            alts
      in  Case (go locals e) alts'

canonicalisePattern :: Env -> Syn.Pattern -> ([RawName], Can.Pattern)
canonicalisePattern env = \case
  VarPat n    -> ([n], VarPat (Local n))
  WildPat     -> ([], WildPat)
  IntPat    i -> ([], IntPat i)
  StringPat s -> ([], StringPat s)
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

canonicaliseName :: Env -> RawName -> Can.Name
canonicaliseName (thisModule, imps) n = case Map.lookup n imps of
  Just i  -> TopLevel i n
  Nothing -> trace
    ("[" <> show thisModule <> "] Could not find " <> show n <> "\n")
    (TopLevel thisModule n)

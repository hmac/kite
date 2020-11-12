module Canonicalise where

import           Prelude                 hiding ( mod )

import qualified Data.Map.Strict               as Map

import           Text.Megaparsec                ( SourcePos )
import           Data.List                      ( mapAccumL )
import           Util
import           Syn
import           AST                            ( Expr(..)
                                                , Pat(..)
                                                )
import           Data.Name                      ( Name(..) )
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

buildImports :: Syn.Module -> Imports
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
flattenImportItem :: Syn.Module -> Syn.ImportItem -> [RawName]
flattenImportItem modul = \case
  ImportSingle { importItemName = n } -> [n]
  ImportAll { importItemName = n } -> n : constructorsForType modul n
  ImportSome { importItemName = n, importItemConstructors = cs } -> n : cs

-- Given a name of a data type, find all the names that would be
-- imported by a (..) style import.
-- For data types, this is all the constructor names.
constructorsForType :: Syn.Module -> RawName -> [RawName]
constructorsForType modul tyname =
  let datas = dataDecls modul
  in  case find ((== tyname) . dataName) datas of
        Just d  -> map conName (dataCons d)
        Nothing -> []

canonicaliseModule :: Syn.Module -> Can.Module
canonicaliseModule m =
  let imports = buildImports m
      env     = (moduleName m, imports)
      exports =
          [ (Local export, map Local subexports)
          | (export, subexports) <- moduleExports m
          ]
  in  m { moduleExports = exports
        , moduleDecls   = map (canonicaliseDecl env) (moduleDecls m)
        }

canonicaliseDecl :: Env -> Syn.Decl Syn.Syn -> Can.Decl Can.Exp
canonicaliseDecl env = \case
  FunDecl   f -> FunDecl $ canonicaliseFun env f
  DataDecl  d -> DataDecl $ canonicaliseData env d
  AliasDecl a -> AliasDecl $ canonicaliseAlias env a
  Comment   s -> Comment s

canonicaliseFun :: Env -> Syn.Fun Syn.Syn -> Can.Fun Can.Exp
canonicaliseFun (mod, imps) f = f
  { funName = TopLevel mod (funName f)
  , funExpr = canonicaliseExp (mod, imps) mempty (funExpr f)
  , funType = canonicaliseType (mod, imps) <$> funType f
  }

canonicaliseType :: Env -> Syn.Type -> Can.Type
canonicaliseType env = \case
  -- Note: type variables are assumed to be local to the type
  -- this may need rethinking when we support type aliases
  TyCon n         -> TyCon (canonicaliseName env n)
  TyApp a b       -> TyApp (canonicaliseType env a) (canonicaliseType env b)
  TyVar v         -> TyVar (Local v)
  TyList          -> TyList
  TyTuple as      -> TyTuple $ fmap (canonicaliseType env) as
  TyHole  n       -> TyHole n
  TyInt           -> TyInt
  TyString        -> TyString
  TyChar          -> TyChar
  TyBool          -> TyBool
  TyUnit          -> TyUnit
  TyFun a b       -> TyFun (canonicaliseType env a) (canonicaliseType env b)
  TyRecord fields -> TyRecord (map (bimap Local (canonicaliseType env)) fields)
  TyAlias  n a    -> TyAlias (canonicaliseName env n) (canonicaliseType env a)
  TyForall v t    -> TyForall (Local v) (canonicaliseType env t)

canonicaliseData :: Env -> Syn.Data -> Can.Data
canonicaliseData (mod, imps) d = d
  { dataName = TopLevel mod (dataName d)
  , dataCons = fmap (canonicaliseDataCon (mod, imps)) (dataCons d)
  }

canonicaliseAlias :: Env -> Syn.Alias -> Can.Alias
canonicaliseAlias (mod, imps) a = a
  { aliasName = TopLevel mod (aliasName a)
  , aliasType = canonicaliseType (mod, imps) (aliasType a)
  }

canonicaliseDataCon :: Env -> Syn.DataCon -> Can.DataCon
canonicaliseDataCon (mod, imps) DataCon { conName = name, conArgs = args } =
  DataCon { conName = TopLevel mod name
          , conArgs = fmap (canonicaliseType (mod, imps)) args
          }

canonicaliseExp :: Env -> [RawName] -> Syn.Syn -> Can.Exp
canonicaliseExp env = go
 where
  go locals = \case
    Var s n | n `elem` locals -> Var s (Local n)
            | otherwise       -> Var s $ canonicaliseName env n
    Ann s e t -> Ann s (canonicaliseExp env locals e) (canonicaliseType env t)
    Con s n | n `elem` locals -> Con s (Local n)
            | otherwise       -> Con s $ canonicaliseName env n
    Abs  s ns    e           -> Abs s (fmap Local ns) $ go (ns <> locals) e
    App  s a     b           -> App s (go locals a) (go locals b)
    Let  s binds e           -> canonicaliseLet s binds e
    Case s e     alts        -> canonicaliseCase s (e, alts)
    MCase    s alts          -> canonicaliseMCase s alts
    TupleLit s es            -> TupleLit s $ fmap (go locals) es
    ListLit  s es            -> ListLit s $ fmap (go locals) es
    StringInterp s pre parts -> StringInterp s pre $ mapFst (go locals) parts
    StringLit s str          -> StringLit s str
    CharLit   s c            -> CharLit s c
    Hole      s n            -> Hole s (canonicaliseName env n)
    IntLit    s i            -> IntLit s i
    BoolLit   s b            -> BoolLit s b
    UnitLit s                -> UnitLit s
    Record s fields          -> Record s $ mapSnd (go locals) fields
    Project s r    l         -> Project s (go locals r) l
    FCall   s proc args      -> FCall s proc (map (go locals) args)
   where
    canonicaliseLet
      :: Maybe (SourcePos, SourcePos)
      -> [(RawName, Syn.Syn, Maybe Syn.Type)]
      -> Syn.Syn
      -> Can.Exp
    canonicaliseLet s binds expr =
      let
        (locals', binds') = foldl
          (\(locals_, acc) (varName, e, ty) ->
            -- Note: to handle recursive lets, here we would extend the set of
            -- locals with the variable name before canonicalising the bound
            -- expression. We currently don't do that because Kite's evaluation
            -- strategy is strict and the naive evaluation of recursive lets
            -- doesn't terminate. I'm not yet sure if we should support
            -- recursive lets at all - maybe you should always write a
            -- separate function if you want recursion?
            let b = (Local varName, go locals_ e, canonicaliseType env <$> ty)
            in  (varName : locals_, b : acc)
          )
          (locals, [])
          binds
      in  Let s binds' (go locals' expr)

    canonicaliseCase
      :: Maybe (SourcePos, SourcePos)
      -> (Syn.Syn, [(Syn.Pattern, Syn.Syn)])
      -> Can.Exp
    canonicaliseCase s (e, alts) =
      let alts' = map
            (\(pat, e_) ->
              let (vars, pat') = canonicalisePattern env pat
                  e'           = go (vars <> locals) e_
              in  (pat', e')
            )
            alts
      in  Case s (go locals e) alts'
    canonicaliseMCase
      :: Maybe (SourcePos, SourcePos) -> [([Syn.Pattern], Syn)] -> Can.Exp
    canonicaliseMCase s alts =
      let alts' = map
            (\(pats, e) ->
              let (vars, pats') = mapAccumL
                    (\vs pat -> first (vs <>) (canonicalisePattern env pat))
                    []
                    pats
                  e' = go (vars <> locals) e
              in  (pats', e')
            )
            alts
      in  MCase s alts'

canonicalisePattern :: Env -> Syn.Pattern -> ([RawName], Can.Pattern)
canonicalisePattern env = \case
  VarPat n       -> ([n], VarPat (Local n))
  WildPat        -> ([], WildPat)
  IntPat  i      -> ([], IntPat i)
  CharPat c      -> ([], CharPat c)
  BoolPat b      -> ([], BoolPat b)
  UnitPat        -> ([], UnitPat)
  StringPat s    -> ([], StringPat s)
  TuplePat  pats -> second TuplePat (canonicalisePatternList pats)
  ListPat   pats -> second ListPat (canonicalisePatternList pats)
  ConsPat c pats ->
    second (ConsPat (canonicaliseName env c)) (canonicalisePatternList pats)
 where
  canonicalisePatternList :: [Syn.Pattern] -> ([RawName], [Can.Pattern])
  canonicalisePatternList pats =
    let res  = map (canonicalisePattern env) pats
        vars = concatMap fst res
    in  (vars, map snd res)

canonicaliseName :: Env -> RawName -> Name
canonicaliseName (thisModule, imps) n = case Map.lookup n imps of
  Just i  -> TopLevel i n
  Nothing -> TopLevel thisModule n

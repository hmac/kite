{-# LANGUAGE TupleSections #-}
module Canonicalise where

import           qualified Data.Map.Strict               as Map

import Util
import Data.Name
import Syntax                        as Syn
import qualified Canonical                     as Can
import Canonical (Name(Local, TopLevel))
import qualified Canonical.Primitive

-- Converts raw names to canonical names.

-- To canonicalise a module we need to know what names are imported from other
-- modules. We represent that as map from name to module.

type Imports = Map.Map Syn.Name Syn.ModuleName
type Env = (ModuleName, Imports)

buildImports :: Syn.Module Syn.Syn -> Imports
buildImports m =
  let imports = concatMap
        (\imp -> fmap (, Syn.importName imp) (Syn.importItems imp))
        (Syn.moduleImports m)
  in  Map.fromList (imports <> Canonical.Primitive.primitives)

canonicaliseModule :: Syn.Module Syn.Syn -> Can.Module Can.Exp
canonicaliseModule m =
  let imports = buildImports m
      env = (Syn.moduleName m, imports)
  in  m
        { Syn.moduleExports = fmap Can.Local (Syn.moduleExports m)
        , Syn.moduleDecls   = fmap (canonicaliseDecl env)
                                   (Syn.moduleDecls m)
        }

canonicaliseDecl :: Env -> Syn.Decl Syn.Syn -> Can.Decl Can.Exp
canonicaliseDecl env = \case
  Syn.FunDecl f -> Syn.FunDecl $ canonicaliseFun env f
  Syn.DataDecl d -> Syn.DataDecl $ canonicaliseData env d
  Syn.TypeclassDecl t -> Syn.TypeclassDecl $ canonicaliseTypeclass env t
  Syn.TypeclassInst i -> Syn.TypeclassInst $ canonicaliseInstance env i
  Syn.Comment s -> Syn.Comment s

canonicaliseFun :: Env -> Syn.Fun Syn.Syn -> Can.Fun Can.Exp
canonicaliseFun (mod, imps) f =
  f { Syn.funName = TopLevel mod (Syn.funName f)
    , Syn.funDefs = fmap (canonicaliseDef (mod, imps)) (Syn.funDefs f)
    , Syn.funConstraint = canonicaliseConstraint (mod, imps) <$> (Syn.funConstraint f)
    }

canonicaliseConstraint :: Env -> Syn.Constraint -> Can.Constraint
canonicaliseConstraint env c = undefined

canonicaliseData :: Env -> Syn.Data -> Can.Data
canonicaliseData (mod, imps) d =
  d { Syn.dataName = TopLevel mod (Syn.dataName d)
    , Syn.dataCons = fmap (canonicaliseDataCon (mod,imps)) (Syn.dataCons d)
    }

canonicaliseDataCon :: Env -> Syn.DataCon -> Can.DataCon
canonicaliseDataCon (mod, imps) d =
  d { Syn.conName = TopLevel mod (Syn.conName d) }

canonicaliseTypeclass :: Env -> Syn.Typeclass -> Can.Typeclass
canonicaliseTypeclass (mod, imps) t =
  t { Syn.typeclassName = TopLevel mod (Syn.typeclassName t)
    , Syn.typeclassDefs = mapFst (TopLevel mod) (Syn.typeclassDefs t)
    }

canonicaliseInstance :: Env -> Syn.Instance Syn.Syn -> Can.Instance Can.Exp
canonicaliseInstance (mod, imps) i = 
  i { Syn.instanceName = TopLevel mod (Syn.instanceName i)
    , Syn.instanceDefs = fmap (bimap (TopLevel mod) (fmap (canonicaliseDef (mod, imps)))) (Syn.instanceDefs i)
    }

canonicaliseDef :: Env -> Syn.Def Syn.Syn -> Can.Def Can.Exp
canonicaliseDef env d =
  let res = map (canonicalisePattern env) (Syn.defArgs d)
      locals = concatMap fst res
      args = map snd res
  in d { Syn.defExpr = canonicaliseExp env locals (Syn.defExpr d)
       , Syn.defArgs = args
       }

canonicaliseExp :: Env -> [Syn.Name] -> Syn.Syn -> Can.Exp
canonicaliseExp (mod,imps) = go
  where go locals = \case
          Var n | n `elem` locals -> Var (Local n)
                | otherwise -> case Map.lookup n imps of
                                 Just i -> Var (TopLevel i n)
                                 Nothing -> Var (TopLevel mod n)
          Cons n | n `elem` locals -> Cons (Local n)
                 | otherwise -> case Map.lookup n imps of
                                  Just i -> Cons (TopLevel i n)
                                  Nothing -> Cons (TopLevel mod n)
          Abs ns e -> Abs (fmap Local ns) $ go (ns <> locals) e
          App a b -> App (go locals a) (go locals b)
          Let binds e -> canonicaliseLet (binds, e)
          Case e alts -> canonicaliseCase (e, alts)
          TupleLit es -> TupleLit $ fmap (go locals) es
          ListLit es -> ListLit $ fmap (go locals) es
          StringLit pre parts -> StringLit pre $ mapFst (go locals) parts
          Hole n -> Hole n
          IntLit i -> IntLit i
          FloatLit f -> FloatLit f
          where
            canonicaliseLet :: ([(Syn.Name, Syn.Syn)], Syn.Syn) -> Can.Exp
            canonicaliseLet  (binds, e) =
              let (locals', binds') = foldl (\(locals, acc) (n, e) ->
                    let b = (Local n, go locals e)
                     in (n:locals, b:acc)) (locals, []) binds
               in Let binds' (go locals' e)

            canonicaliseCase :: (Syn.Syn, [(Pattern, Syn.Syn)]) -> Can.Exp
            canonicaliseCase (e, alts) =
              let alts' = map (\(pat, e) ->
                                    let (vars, pat') = canonicalisePattern (mod, imps) pat
                                        e' = go (vars <> locals) e
                                    in (pat', e')) alts
             in 
              Case (go locals e) alts'

canonicalisePattern :: Env -> Syn.Pattern -> ([Syn.Name], Can.Pattern)
canonicalisePattern (mod, imps) = \case
  VarPat n -> ([n], VarPat (Local n))
  WildPat -> ([], WildPat)
  IntPat i -> ([], IntPat i)
  TuplePat pats ->
    let res = map (canonicalisePattern (mod, imps)) pats
        vars = concatMap fst res
        pats' = map snd res
     in (vars, TuplePat pats')
  ListPat pats ->
    let res = map (canonicalisePattern (mod, imps)) pats
        vars = concatMap fst res
        pats' = map snd res
     in (vars, ListPat pats')
  ConsPat c pats ->
    let c' = case Map.lookup c imps of
               Just i -> TopLevel i c
               Nothing -> TopLevel mod c
        res = map (canonicalisePattern (mod,imps)) pats
        vars = concatMap fst res
        pats' = map snd res
     in (vars, ConsPat c' pats')


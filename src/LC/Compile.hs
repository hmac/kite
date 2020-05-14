module LC.Compile
  ( runConvert
  , convertEnv
  , Env
  )
where

import           Data.List                      ( nub )
import           Data.Foldable                  ( foldrM )

import           LC
import           ELC                            ( Con(..)
                                                , Pattern(..)
                                                , Clause(..)
                                                )
import qualified ELC
import qualified ELC.Compile                    ( Env
                                                , collapseEnv
                                                )
import           Data.Name
import           NameGen                        ( NameGen
                                                , freshM
                                                , run
                                                )

fresh :: NameGen Name
fresh = freshM (\i -> Local (Name ("$lc" ++ show i)))

runConvert :: NameGen b -> b
runConvert = run

convertEnv :: ELC.Compile.Env -> NameGen Env
convertEnv env = mapSndM convert (ELC.Compile.collapseEnv env)

convert :: ELC.Exp -> NameGen Exp
convert = go
 where
  go (ELC.Var n           )  = pure (Var n)
  go (ELC.Cons  c es      )  = Cons c <$> mapM go es
  go (ELC.Const c es      )  = Const c <$> mapM go es
  go (ELC.App   a b       )  = App <$> go a <*> go b
  go (ELC.Abs   p e       )  = convertAbs p e
  go (ELC.Let pat bind e  )  = convertLet pat bind e
  go (ELC.LetRec alts e   )  = convertLetRec alts e
  go (ELC.Fatbar a    b   )  = Fatbar <$> go a <*> go b
  go (ELC.Case   n    alts)  = convertCase n alts
  go ELC.Fail                = pure Fail
  go (ELC.Bottom s         ) = pure (Bottom s)
  go (ELC.Project a i e    ) = Project a i <$> go e
  go (ELC.Y      e         ) = Y <$> go e
  go (ELC.Record fields    ) = Record <$> traverse go fields
  go (ELC.RecordProject e l) = RecordProject <$> go e <*> pure l

convertAbs :: Pattern -> ELC.Exp -> NameGen Exp
-- constant patterns
convertAbs (ConstPat c) e = do
  e' <- convert e
  v  <- fresh
  pure $ Abs v (If (Eq (Var v) (Const c [])) e' Fail)
convertAbs (VarPat v                         ) e = Abs v <$> convert e
convertAbs (ConPat Prod { conArity = a } pats) e = do
  lam <- convert (ELC.buildAbs e pats)
  f   <- fresh
  pure $ Abs f (UnpackProduct a lam (Var f))
convertAbs (ConPat Sum { sumTag = t, conArity = a } pats) e = do
  lam <- convert (ELC.buildAbs e pats)
  f   <- fresh
  pure $ Abs f (UnpackSum t a lam (Var f))

convertLet :: Pattern -> ELC.Exp -> ELC.Exp -> NameGen Exp
convertLet (VarPat n) val body = Let n <$> convert val <*> convert body
convertLet pat        val body = do
  (pat', val') <- convertRefutableLetBinding (pat, val)
  (n, v, e)    <- convertIrrefutableLet pat' val' body
  convertSimpleELCLet n v e

convertLetRec :: [(Pattern, ELC.Exp)] -> ELC.Exp -> NameGen Exp
convertLetRec alts body = do
  alts'          <- mapM convertRefutableLetBinding alts
  irrefutableLet <- irrefutableLetRec2IrrefutableLet alts' body
  case irrefutableLet of
    ELC.Let pat val body' -> do
      (n, v, e) <- convertIrrefutableLet pat val body'
      convertSimpleELCLet n v e
    other ->
      error $ "LC.Compile.convertLetRec: expected let but found " <> show other

--------------------------------------------------------------------------------
-- Definition: Irrefutable Pattern
--------------------------------------------------------------------------------
-- A pattern p is irrefutable iff it is either:
-- * a variable v
-- * a product pattern of the form (t p1...pr) where p1...pr are irrefutable
--   patterns
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Definition: Simple let(rec)
--------------------------------------------------------------------------------
-- A let(rec) is simple iff the left hand side of each definition is a variable.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Definition: Irrefutable let(rec)
--------------------------------------------------------------------------------
-- A let(rec) is irrefutable iff the left hand side of each definition is an
-- irrefutable pattern.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Definition: General let(rec)
--------------------------------------------------------------------------------
-- A general let(rec) can have any arbitrary pattern on the left hand side.
--------------------------------------------------------------------------------

-- This isn't used but it's an informative definition
--
-- isSimple :: ELC.Exp -> Bool
-- isSimple (ELC.Let (VarPat _) v e) = isSimple v && isSimple e
-- isSimple ELC.Let{}                = False
-- isSimple _                        = True
-- 
-- How we transform let(rec)s:
-- ---------------------------
-- refutable letrec -> irrefutable letrec -> irrefutable let -> simple let -> lambda
-- refutable let    -> irrefutable let                       -> simple let -> lambda

-- Convert a simple ELC let expression to an LC let expression
convertSimpleELCLet :: Name -> ELC.Exp -> ELC.Exp -> NameGen Exp
convertSimpleELCLet n v e = Let n <$> convert v <*> convert e

-- Convert an irrefutable let expression to a simple let expression
-- TODO: make sure we can't encounter constant patterns here, and try to express
-- that in the type.
convertIrrefutableLet
  :: Pattern -> ELC.Exp -> ELC.Exp -> NameGen (Name, ELC.Exp, ELC.Exp)
convertIrrefutableLet (ConPat Prod { conArity = a } pats) val body = do
  var <- fresh
  let patBinds =
        zipWith (\p i -> (p, ELC.Project a i (ELC.Var var))) pats [0 ..]
  body' <- foldrM
    (\(p, v) acc -> do
      (n, value, e) <- convertIrrefutableLet p v acc
      pure $ ELC.Let (VarPat n) value e
    )
    body
    patBinds
  pure (var, val, body')
convertIrrefutableLet (VarPat v) val body = pure (v, val, body)
convertIrrefutableLet p _ _ = error $ "Unexpected pattern " <> show p

-- Convert an irrefutable letrec to an irrefutable let
irrefutableLetRec2IrrefutableLet
  :: [(Pattern, ELC.Exp)] -> ELC.Exp -> NameGen ELC.Exp
irrefutableLetRec2IrrefutableLet alts body = do
  tupleName <- fresh
  let pats  = map fst alts
  let con = Prod { conName = tupleName, conArity = length pats }
  let tuple = ELC.Cons con (map snd alts)
  let pat   = ConPat con pats
  pure $ ELC.Let pat (ELC.Y (ELC.Abs pat tuple)) body

-- Convert a refutable let(rec) binding to an irrefutable binding
convertRefutableLetBinding :: (Pattern, ELC.Exp) -> NameGen (Pattern, ELC.Exp)
convertRefutableLetBinding (pat, val) = do
  tupleName <- fresh
  varName   <- fresh
  let vars  = extractPatternVars pat
  let con   = Prod { conName = tupleName, conArity = length vars }
  let tuple = ELC.Cons con (map ELC.Var vars)
  let pat'  = ConPat con (map VarPat vars)
  let rhs = ELC.Let
        (VarPat varName)
        val
        (ELC.Fatbar (ELC.App (ELC.Abs pat tuple) val)
                    (ELC.Bottom "pattern match failure")
        )
  pure (pat', rhs)

-- Extract all variables bound in a pattern.
-- Note: we remove duplicate pattern variables
-- Lam should disallow duplicate variables in a pattern but that's not yet
-- implemented.
extractPatternVars :: Pattern -> [Name]
extractPatternVars (VarPat   v   ) = [v]
extractPatternVars (ConstPat _   ) = []
extractPatternVars (ConPat _ pats) = nub $ concatMap extractPatternVars pats

-- TODO: dependency analysis (§6.2.8)

convertCase :: Name -> [Clause] -> NameGen Exp
-- Product types:
-- case v of         ==> let v1 = PROJECT 1 v
--   t v1...vn -> E          ...
--                           vn = PROJECT n v
--                        in E
convertCase n [Clause p@Prod{} vars body] =
  unpackClause (Var n) (Clause p vars body)
-- Sum types:
convertCase varName clauses = do
  branches <- mapM (unpackClause (Var varName)) clauses
  pure $ CaseN (Var varName) branches

unpackClause :: Exp -> Clause -> NameGen Exp
unpackClause scrut (Clause c vars body) = do
  body' <- convert body
  let binds =
        zipWith (\v i -> Let v (Project (conArity c) i scrut)) vars [0 ..]
  pure $ if null binds then body' else foldl1 (.) binds body'

mapSndM :: Monad m => (b -> m c) -> [(a, b)] -> m [(a, c)]
mapSndM _ []            = return []
mapSndM f ((a, b) : xs) = do
  c  <- f b
  rs <- mapSndM f xs
  return ((a, c) : rs)

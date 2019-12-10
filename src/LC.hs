module LC where

-- Simple lambda calculus

import           Syntax                         ( Name(..) )
import           ELC                            ( Constant(..)
                                                , Con(..)
                                                , Pattern(..)
                                                , Clause(..)
                                                )
import qualified ELC

import           Control.Monad.State.Strict
import           Data.List                      ( nub )
import           Data.Foldable                  ( foldrM )
import           Control.Monad.Extra            ( mconcatMapM )

data Exp = Const Constant
         | Var Name
         | Cons Con [Exp]
         | App Exp Exp
         | Abs Name Exp
         | Bottom String
         | Fail
         | Fatbar Exp Exp
         | ETrue
         | EFalse
         | If Exp Exp Exp
         | Eq Exp Exp
         -- UnpackProduct i f a  expects a to be a product of arity i, unpacks
         -- it, and passes the values inside it as arguments to f
         | UnpackProduct Int Exp Exp
         -- UnpackSum t i f a  expects a to be a sum with tag t, arity i
         -- it unpacks it and passes the values insite it as arguments to f
         | UnpackSum Int Int Exp Exp
         | Project Int Int Exp   -- arity of the constructor; index of the field
         -- The Y combinator
         | Y Exp
         deriving (Eq, Show)

type NameGen = State Int

fresh :: NameGen Name
fresh = do
  k <- get
  put (k + 1)
  pure $ Name $ "$lc" ++ show k

runConvert :: ELC.Exp -> Exp
runConvert e = evalState (convert e) 0

convert :: ELC.Exp -> NameGen Exp
convert = go
 where
  go (ELC.Var n           ) = pure (Var n)
  go (ELC.Cons c es       ) = Cons c <$> mapM go es
  go (ELC.Const c         ) = pure (Const c)
  go (ELC.App a b         ) = App <$> go a <*> go b
  go (ELC.Abs p e         ) = convertAbs p e
  go (ELC.Let pat bind e  ) = convertLet pat bind e
  go (ELC.LetRec alts e   ) = convertLetRec alts e
  go (ELC.Fatbar a    b   ) = Fatbar <$> go a <*> go b
  go (ELC.Case   n    alts) = convertCase n alts
  go ELC.Fail               = pure Fail
  go (ELC.Bottom s     )    = pure (Bottom s)
  go (ELC.Project a i e)    = Project a i <$> go e
  go (ELC.Y e          )    = Y <$> go e

convertAbs :: Pattern -> ELC.Exp -> NameGen Exp
-- constant patterns
convertAbs (ConstPat c) e = do
  e' <- convert e
  v  <- fresh
  pure $ Abs v (If (Eq (Var v) (Const c)) e' Fail)
convertAbs (VarPat v                      ) e = Abs v <$> convert e
convertAbs (ConPat Prod { arity = a } pats) e = do
  lam <- convert (ELC.buildAbs e pats)
  f   <- fresh
  pure $ Abs f (UnpackProduct a lam (Var f))
convertAbs (ConPat Sum { tag = t, arity = a } pats) e = do
  lam <- convert (ELC.buildAbs e pats)
  f   <- fresh
  pure $ Abs f (UnpackSum t a lam (Var f))

convertLet :: Pattern -> ELC.Exp -> ELC.Exp -> NameGen Exp
convertLet (VarPat n) val body = convertSimpleLet n val body
convertLet pat        val body = do
  (pat', val') <- convertRefutableLetBinding (pat, val)
  simple       <- convertIrrefutableLet pat' val' body
  let ELC.Let (VarPat n) v e = simple
  convertSimpleLet n v e

convertLetRec :: [(Pattern, ELC.Exp)] -> ELC.Exp -> NameGen Exp
convertLetRec alts body = do
  alts'          <- mapM convertRefutableLetBinding alts
  irrefutableLet <- irrefutableLetRec2IrrefutableLet alts' body
  let ELC.Let pat val body' = irrefutableLet
  simple <- convertIrrefutableLet pat val body'
  let ELC.Let (VarPat n) v e = simple
  convertSimpleLet n v e

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

isSimple :: ELC.Exp -> Bool
isSimple (ELC.Let (VarPat _) v e) = isSimple v && isSimple e
isSimple ELC.Let{}                = False
isSimple _                        = True

-- How we transform let(rec)s:
-- ---------------------------
-- refutable letrec -> irrefutable letrec -> irrefutable let -> simple let -> lambda
-- refutable let    -> irrefutable let                       -> simple let -> lambda

-- Convert a simple let expression to a lambda abstraction
convertSimpleLet :: Name -> ELC.Exp -> ELC.Exp -> NameGen Exp
convertSimpleLet v val body = do
  val'  <- convert val
  body' <- convert body
  pure $ App (Abs v body') val'

-- Convert an irrefutable let expression to a simple let expression
convertIrrefutableLet :: Pattern -> ELC.Exp -> ELC.Exp -> NameGen ELC.Exp
convertIrrefutableLet (ConPat Prod { arity = a } pats) val body = do
  var <- fresh
  let patBinds =
        zipWith (\p i -> (p, ELC.Project a i (ELC.Var var))) pats [0 ..]
  body' <- foldrM (\(p, v) acc -> convertIrrefutableLet p v acc) body patBinds
  pure $ ELC.Let (VarPat var) val body'
convertIrrefutableLet p val body = pure $ ELC.Let p val body

-- Convert an irrefutable letrec expression to a simple letrec expression
convertIrrefutableLetRec :: [(Pattern, ELC.Exp)] -> ELC.Exp -> NameGen ELC.Exp
convertIrrefutableLetRec alts body = do
  alts' <- mconcatMapM convertSinglePattern alts
  pure $ ELC.LetRec alts' body
 where
  convertSinglePattern :: (Pattern, ELC.Exp) -> NameGen [(Pattern, ELC.Exp)]
  convertSinglePattern (ConPat Prod { arity = a } pats, val) = do
    var <- fresh
    let patBinds =
          zipWith (\p i -> (p, ELC.Project a i (ELC.Var var))) pats [0 ..]
    patBinds' <- mconcatMapM convertSinglePattern patBinds
    pure $ (VarPat var, val) : patBinds'
  convertSinglePattern (p, val) = pure [(p, val)]

-- Convert an irrefutable letrec to an irrefutable let
irrefutableLetRec2IrrefutableLet
  :: [(Pattern, ELC.Exp)] -> ELC.Exp -> NameGen ELC.Exp
irrefutableLetRec2IrrefutableLet alts body = do
  tupleName <- fresh
  let pats  = map fst alts
  let con = Prod { name = tupleName, arity = length pats }
  let tuple = ELC.Cons con (map snd alts)
  let pat   = ConPat con pats
  pure $ ELC.Let pat (ELC.Y (ELC.Abs pat tuple)) body

-- Convert a refutable let(rec) binding to an irrefutable binding
convertRefutableLetBinding :: (Pattern, ELC.Exp) -> NameGen (Pattern, ELC.Exp)
convertRefutableLetBinding (pat, val) = do
  tupleName <- fresh
  varName   <- fresh
  let vars  = extractPatternVars pat
  let con   = Prod { name = tupleName, arity = length vars }
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
convertCase = undefined

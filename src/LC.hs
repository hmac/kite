module LC where

-- Simple lambda calculus

import           Syntax                         ( Name(..) )
import           ELC                            ( Constant(..)
                                                , Con(..)
                                                , Pattern(..)
                                                )
import qualified ELC

import           Control.Monad.State.Strict

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
         deriving (Eq, Show)

type NameGen = State Int

fresh :: NameGen Name
fresh = do
  k <- get
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
  go (ELC.Fatbar a    b   ) = Fatbar <$> convert a <*> convert b
  go (ELC.Case   n    alts) = convertCase n alts
  go ELC.Fail               = pure Fail
  go (ELC.Bottom s)         = pure (Bottom s)

convertAbs :: ELC.Pattern -> ELC.Exp -> NameGen Exp
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

convertLet :: ELC.Pattern -> ELC.Exp -> ELC.Exp -> NameGen Exp
convertLet = undefined

convertLetRec :: [(ELC.Pattern, ELC.Exp)] -> ELC.Exp -> NameGen Exp
convertLetRec = undefined

convertCase :: Name -> [(ELC.Pattern, ELC.Exp)] -> NameGen Exp
convertCase = undefined

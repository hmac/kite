module LC.Eval where

import ELC (Con(..), Constant(..), Primitive(..))
import           LC
import           Syntax                         ( Name(..) )

type Env = [(Name, Exp)]

evalMain :: Env -> Exp
evalMain = evalVar "main"

evalVar :: Name -> Env -> Exp
evalVar n env = eval env (Var n)

eval :: Env -> Exp -> Exp
eval env = \case
  Const c es -> evalConst env c es
  Var n -> case lookup n env of
             Just e -> eval env e
             Nothing -> error $ "unknown variable: " <> show n
  Cons c es -> Cons c es
  -- note that we evaluate the argument before substituting it - i.e. strict
  -- evaluation.
  App (Abs n e) b -> eval env $ subst (eval env b) n e
  App a b -> eval env $ App (eval env a) b
  Abs n e -> Abs n e
  Let n v e -> eval ((n,v) : env) e
  Bottom s -> Bottom s
  Fail -> Fail
  Fatbar a b -> case eval env a of
    Fail       -> eval env b
    (Bottom s) -> Bottom s
    e          -> e
  ETrue -> ETrue
  EFalse -> EFalse
  If c t e -> case eval env c of
                ETrue -> eval env t
                EFalse -> eval env e
                a -> error $ "Expected boolean but got " <> show a
  Eq a b -> let a' = eval env a
                b' = eval env b
             in if a' == b' then ETrue else EFalse
  UnpackProduct i f (Cons c args) -> if arity c == i
                                        then eval env (buildApp f args)
                                        else error $ "expected " <> show c <> " to have arity " <> show i
  UnpackProduct i f e -> UnpackProduct i f (eval env e)
  UnpackSum t i f (Cons c@Sum { tag = t', arity = i' } args) | t == t' && i == i' -> eval env (buildApp f args)
                                                             | otherwise -> error $ "expected " <> show c <> " to have arity " <> show i <> " and tag " <> show t
  UnpackSum t i f e -> eval env $ UnpackSum t i f (eval env e)
  Project _a i (Cons _ args) -> eval env (args !! i)
  Project a i e -> eval env $ Project a i (eval env e)
  Y e -> eval env $ App e (Y e)
  CaseN _ (Cons Sum { tag = t } _) branches -> eval env (branches !! t)
  CaseN n e branches -> eval env $ CaseN n (eval env e) branches

evalConst :: Env -> Constant -> [Exp] -> Exp
evalConst env (Prim f) args = evalPrim f args
evalConst _ c es = Const c es

evalPrim :: Primitive -> [Exp] -> Exp
evalPrim PrimStringAppend [Const (String a) _, Const (String b) _] = Const (String (a <> b)) []
evalPrim PrimShow [e] = primShow e
evalPrim PrimAdd [Const (Int x) _, Const (Int y) _] = Const (Int (x + y)) []
evalPrim PrimSub [Const (Int x) _, Const (Int y) _] = Const (Int (x - y)) []
evalPrim PrimMult [Const (Int x) _, Const (Int y) _] = Const (Int (x * y)) []

primShow :: Exp -> Exp
primShow Fail       = Fail
primShow (Bottom s) = Bottom s
primShow e          = Const (String (go e)) []
 where
  go (Const (Int    i) _   ) = show i
  go (Const (Float  f) _   ) = show f
  go (Const (String s) _   ) = s
  go (Const (Prim   _) _   ) = "<builtin>"
  go (Abs   _          _   ) = "<function>"
  go (Cons c args) = let Name n = name c in n <> " " <> unwords (map go args)
  go _                       = "<unevaluated>"

subst :: Exp -> Name -> Exp -> Exp
subst a n (Const c es) = Const c (map (subst a n) es)
subst a n (Var m) | n == m    = a
                  | otherwise = Var m
subst a n (Cons c es) = Cons c (map (subst a n) es)
subst a n (App  x y ) = App (subst a n x) (subst a n y)
subst a n (Abs p e) | p == n = Abs p e
                    | otherwise   = Abs p (subst a n e)
subst a n (Let p b e) | p == n = Let p b e
                      | otherwise   = Let p (subst a n b) (subst a n e)
subst a n (Fatbar x y) = Fatbar (subst a n x) (subst a n y)
subst _a _n Fail             = Fail
subst _a _n (Bottom s      ) = Bottom s
subst a  n  (Project ar i e) = Project ar i (subst a n e)
subst a  n  (Y e           ) = Y (subst a n e)
subst _ _ ETrue = ETrue
subst _ _ EFalse = EFalse
subst a n (If c t e) = If (subst a n c) (subst a n t) (subst a n e)
subst a n (Eq x y) = Eq (subst a n x) (subst a n y)
subst a n (UnpackProduct i x y) = UnpackProduct i (subst a n x) (subst a n y)
subst a n (UnpackSum t i x y) = UnpackSum t i (subst a n x) (subst a n y)
subst a n (CaseN i x ys) = CaseN i (subst a n x) (map (subst a n) ys)

buildApp :: Exp -> [Exp] -> Exp
buildApp = foldl App

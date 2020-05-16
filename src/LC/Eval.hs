module LC.Eval where

import qualified Data.Map.Strict               as Map
import           Data.Name
import           ELC                            ( Con(..)
                                                , Constant(..)
                                                , Primitive(..)
                                                )
import           LC

evalMain :: ModuleName -> Env -> Exp
evalMain mn = evalVar (TopLevel mn "main")

evalVar :: Name -> Env -> Exp
evalVar n env = eval env (Var n)

eval :: Env -> Exp -> Exp
eval env expr = case expr of
  Const c es -> evalConst env c es
  Var n      -> case lookup n env of
    Just e  -> eval env e
    Nothing -> error $ "unknown variable: " <> show n
  Cons c         es -> Cons c es
  -- note that we evaluate the argument before substituting it - i.e. strict
  -- evaluation.
  App  (Abs n e) b  -> eval env $ subst (eval env b) n e
  App  a         b  -> eval env $ App (eval env a) b
  Abs  n         e  -> Abs n e
  Let n v e         -> eval env $ subst v n e
  Bottom s          -> Bottom s
  Fail              -> Fail
  Fatbar a b        -> case eval env a of
    Fail       -> eval env b
    (Bottom s) -> Bottom s
    e          -> e
  If c t e -> case eval env c of
    Const (Bool True ) _ -> eval env t
    Const (Bool False) _ -> eval env e
    a                    -> error $ "Expected boolean but got " <> show a
  Eq a b ->
    let a' = eval env a
        b' = eval env b
    in  if a' == b' then Const (Bool True) [] else Const (Bool False) []
  UnpackProduct i f (Cons c args) -> if conArity c == i
    then eval env (buildApp f args)
    else error $ "expected " <> show c <> " to have arity " <> show i
  UnpackProduct i f e -> UnpackProduct i f (eval env e)
  UnpackSum t i f (Cons c@Sum { sumTag = t', conArity = i' } args)
    | t == t' && i == i'
    -> eval env (buildApp f args)
    | otherwise
    -> error
      $  "expected "
      <> show c
      <> " to have arity "
      <> show i
      <> " and tag "
      <> show t
  UnpackSum t i f e               -> eval env $ UnpackSum t i f (eval env e)
  Project _a i (Cons _ args)      -> eval env (args !! i)
  Project a  i e                  -> eval env $ Project a i (eval env e)
  Y e                             -> eval env $ App e (Y e)
  CaseN (Cons Sum { sumTag = t } _) branches -> eval env (branches !! t)
  CaseN e branches                -> eval env $ CaseN (eval env e) branches
  Record fields                   -> Record fields
  RecordProject (Record fields) l -> case Map.lookup l fields of
    Just e -> eval env e
    Nothing ->
      error
        $  "Eval: expected "
        <> show (Record fields)
        <> "to have field "
        <> show l
  RecordProject e l -> eval env $ RecordProject (eval env e) l

evalConst :: Env -> Constant -> [Exp] -> Exp
evalConst _ (Prim f) args = evalPrim f args
evalConst _ c        es   = Const c es

evalPrim :: Primitive -> [Exp] -> Exp
evalPrim PrimStringAppend [] = Const (String "") []
evalPrim PrimStringAppend [Const (String a) _, Const (String b) _] =
  Const (String (a <> b)) []

evalPrim PrimAdd     [Const (Int x) _, Const (Int y) _] = Const (Int (x + y)) []
evalPrim PrimSub     [Const (Int x) _, Const (Int y) _] = Const (Int (x - y)) []
evalPrim PrimMult    [Const (Int x) _, Const (Int y) _] = Const (Int (x * y)) []
evalPrim PrimShow    [e              ]                  = primShow e
evalPrim PrimShowInt [Const (Int x) _] = Const (String (show x)) []
evalPrim PrimEqInt [Const (Int x) _, Const (Int y) _]
  | x == y    = Const (Bool True) []
  | otherwise = Const (Bool False) []

evalPrim p args =
  error $ "(LC.Eval) [" <> show p <> "] invalid args: " <> show args

primShow :: Exp -> Exp
primShow Fail       = Fail
primShow (Bottom s) = Bottom s
primShow e          = Const (String (go e)) []
 where
  go (Const (Int    i) _   ) = show i
  go (Const (String s) _   ) = s
  go (Const (Prim   _) _   ) = "<builtin>"
  go (Abs   _          _   ) = "<function>"
  go (Cons  c          args) = case conName c of
    TopLevel _ (Name n) -> n <> " " <> unwords (map go args)
    n                   -> error $ "primShow: unexpected conName " <> show n
  go _ = "<unevaluated>"

subst :: Exp -> Name -> Exp -> Exp
subst a n (Const c es) = Const c (map (subst a n) es)
subst a n (Var m) | n == m    = a
                  | otherwise = Var m
subst a n (Cons c es) = Cons c (map (subst a n) es)
subst a n (App  x y ) = App (subst a n x) (subst a n y)
subst a n (Abs p e) | p == n    = Abs p e
                    | otherwise = Abs p (subst a n e)
subst a n (Let p b e) | p == n    = Let p b e
                      | otherwise = Let p (subst a n b) (subst a n e)
subst a  n  (Fatbar x y)          = Fatbar (subst a n x) (subst a n y)
subst _a _n Fail                  = Fail
subst _a _n (Bottom s           ) = Bottom s
subst a  n  (Project ar i e     ) = Project ar i (subst a n e)
subst a  n  (Y e                ) = Y (subst a n e)
subst a  n  (If c t e           ) = If (subst a n c) (subst a n t) (subst a n e)
subst a  n  (Eq x y             ) = Eq (subst a n x) (subst a n y)
subst a  n  (UnpackProduct i x y) = UnpackProduct i (subst a n x) (subst a n y)
subst a  n  (UnpackSum t i x y  ) = UnpackSum t i (subst a n x) (subst a n y)
subst a  n  (CaseN x ys         ) = CaseN (subst a n x) (map (subst a n) ys)
subst a  n  (Record fields      ) = Record (fmap (subst a n) fields)
subst a  n  (RecordProject e l  ) = RecordProject (subst a n e) l

buildApp :: Exp -> [Exp] -> Exp
buildApp = foldl App

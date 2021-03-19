{-# LANGUAGE FlexibleContexts #-}
module LC.Eval where

-- import           Util
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                , runReader
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Name
import           ELC                            ( Con(..)
                                                , Constant(..)
                                                , Primitive(..)
                                                )
import           ELC.Primitive                  ( listCons
                                                , listNil
                                                )
import           LC

evalMain :: ModuleName -> Env -> Exp
evalMain mn = evalVar (TopLevel mn "main")

evalVar :: Name -> Env -> Exp
evalVar n env = eval env (Var n)

-- TODO: this might be nicer if we have a new type for normal forms
eval :: Env -> Exp -> Exp
eval env = flip runReader env . go
 where
  go :: MonadReader Env m => Exp -> m Exp
  go expr = case expr of
    Const c es -> pure $ evalConst c es
    Var n      -> asks (lookup n) >>= \case
      Just e  -> go e
      Nothing -> error $ "unknown variable: " <> show n
    Cons c         es -> pure $ Cons c es
    -- note that we evaluate the argument before substituting it - i.e. strict
    -- evaluation.
    App  (Abs n e) b  -> go b >>= \b' -> go (subst b' n e)
    App  a         b  -> go a >>= \a' -> go (App a' b)
    Abs  n         e  -> pure $ Abs n e
    Let n v e         -> go $ subst v n e
    Bottom s          -> pure $ Bottom s
    Fail              -> pure Fail
    Fatbar a b        -> go a >>= \case
      Fail       -> go b
      (Bottom s) -> pure $ Bottom s
      e          -> pure e
    If c t e -> go c >>= \case
      Const (Bool True ) _ -> go t
      Const (Bool False) _ -> go e
      a                    -> error $ "Expected boolean but got " <> show a
    Eq a b -> do
      a' <- go a
      b' <- go b
      pure $ if a' == b' then Const (Bool True) [] else Const (Bool False) []
    UnpackProduct i f (Cons c args) -> if conArity c == i
      then go (buildApp f args)
      else error $ "expected " <> show c <> " to have arity " <> show i
    UnpackProduct i f e -> UnpackProduct i f <$> go e
    UnpackSum t i f (Cons Sum { sumTag = t', conArity = i' } args)
      | t == t' && i == i' -> go (buildApp f args)
      | otherwise          -> pure Fail
    UnpackSum t i f e               -> go . UnpackSum t i f =<< go e
    Project _a i (Cons _ args)      -> go (args !! i)
    Project a  i e                  -> go . Project a i =<< go e
    Y e                             -> go $ App e (Y e)
    CaseN (Cons Sum { sumTag = t } _) branches -> go (branches !! t)
    CaseN e branches                -> go e >>= \e' -> go $ CaseN e' branches
    Record fields                   -> pure $ Record fields
    RecordProject (Record fields) l -> case Map.lookup l fields of
      Just e -> go e
      Nothing ->
        error
          $  "Eval: expected "
          <> show (Record fields)
          <> "to have field "
          <> show l
    RecordProject e    l    -> go e >>= \e' -> go $ RecordProject e' l
    FCall         proc args -> FCall proc <$> mapM go args

evalConst :: Constant -> [Exp] -> Exp
evalConst (Prim f) args = evalPrim f args
evalConst c        es   = Const c es

evalPrim :: Primitive -> [Exp] -> Exp
evalPrim PrimStringAppend [] = Const (String "") []
evalPrim PrimStringAppend [Const (String a) _, Const (String b) _] =
  Const (String (a <> b)) []
evalPrim PrimStringChars [Const (String s) _] =
  foldr (\c cs -> Cons listCons [Const (Char c) [], cs]) (Cons listNil []) s
evalPrim PrimStringConsChar [Const (Char c) _, Const (String s) _] =
  Const (String (c : s)) []
evalPrim PrimStringUnconsChar [Const (String s) _, def, f] = case s of
  c : s' -> App (App f (Const (Char c) [])) (Const (String s') [])
  []     -> def

evalPrim PrimAdd [Const (Int x) _, Const (Int y) _] = Const (Int (x + y)) []
evalPrim PrimSub [Const (Int x) _, Const (Int y) _] = Const (Int (x - y)) []
evalPrim PrimMult [Const (Int x) _, Const (Int y) _] = Const (Int (x * y)) []
evalPrim PrimShow [e] = primShow e
evalPrim PrimShowInt [Const (Int x) _] = Const (String (show x)) []
evalPrim PrimShowChar [Const (Char x) _] = Const (String [x]) []
evalPrim PrimEqInt [Const (Int x) _, Const (Int y) _]
  | x == y    = Const (Bool True) []
  | otherwise = Const (Bool False) []
evalPrim PrimEqChar [Const (Char x) _, Const (Char y) _]
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
subst a  n  (Fatbar x y)              = Fatbar (subst a n x) (subst a n y)
subst _a _n Fail                      = Fail
subst _a _n (Bottom s               ) = Bottom s
subst a  n  (Project ar i e         ) = Project ar i (subst a n e)
subst a  n  (Y e                    ) = Y (subst a n e)
subst a n (If c t e) = If (subst a n c) (subst a n t) (subst a n e)
subst a  n  (Eq x y                 ) = Eq (subst a n x) (subst a n y)
subst a n (UnpackProduct i x y) = UnpackProduct i (subst a n x) (subst a n y)
subst a n (UnpackSum t i x y) = UnpackSum t i (subst a n x) (subst a n y)
subst a  n  (CaseN x ys             ) = CaseN (subst a n x) (map (subst a n) ys)
subst a  n  (Record fields          ) = Record (fmap (subst a n) fields)
subst a  n  (RecordProject e    l   ) = RecordProject (subst a n e) l
subst a  n  (FCall         proc args) = FCall proc (map (subst a n) args)

buildApp :: Exp -> [Exp] -> Exp
buildApp = foldl App

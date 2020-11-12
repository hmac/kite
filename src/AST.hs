module AST
  ( Expr(..)
  , ExprT(..)
  , Pat(..)
  )
where

import           Data.Data                      ( Data )
import           Util                           ( Debug(debug) )
import           Data.List                      ( intercalate )

-- TODO: patterns in let bindings
-- TODO: type sigs in let bindings
-- TODO: multi-definition functions in let bindings
--       (e.g. let fib 0 = 1; fib 1 = 1; fib n = ...)
data Expr l n t = Var l n
         | Ann l (Expr l n t) t
         | Con l n
         | Hole l n
         | Abs l [n] (Expr l n t)
         | App l (Expr l n t) (Expr l n t)
         | Let l [(n, Expr l n t, Maybe t)] (Expr l n t)
         | Case l (Expr l n t) [(Pat n, Expr l n t)]
         | MCase l [([Pat n], Expr l n t)]
         | UnitLit l
         | TupleLit l [Expr l n t]
         | ListLit l [Expr l n t]
         | StringInterp l String [(Expr l n t, String)]
         | StringLit l String
         | CharLit l Char
         | IntLit l Int
         | BoolLit l Bool
         | Record l [(String, Expr l n t)]
         | Project l (Expr l n t) String
         | FCall l String [Expr l n t]
         deriving (Eq, Show, Data)

instance (Debug v, Debug t) => Debug (Expr l v t) where
  debug (Var _ v  ) = debug v
  debug (Ann _ e t) = debug e <+> ":" <+> debug t
  debug (App _ a b) = debug a <+> debug b
  debug (Abs _ v e) = "λ" <> debug v <> "." <+> debug e
  debug (Con  _ v ) = debug v
  debug (Hole _ s ) = "?" <> debug s
  debug (Case _ e alts) =
    "case" <+> debug e <+> "of {" <+> sepBy "; " (map go alts) <+> "}"
    where go (pat, expr) = debug pat <+> "->" <+> debug expr
  debug (MCase _ alts) = "mcase" <+> "{" <+> sepBy "; " (map go alts) <+> "}"
    where go (pats, expr) = sepBy " " (map debug pats) <+> "->" <+> debug expr
  debug (Let _ binds e) =
    "let"
      <+> foldl
            (\acc (x, a, t) -> case t of
              Just t' ->
                debug x <+> ":" <+> debug t' <+> "=" <+> debug a <> ";" <+> acc
              Nothing -> debug x <+> "=" <+> debug a <> ";" <+> acc
            )
            ("in" <+> debug e)
            binds
  debug (StringLit _ s) = "\"" <> s <> "\""
  debug (StringInterp _ s comps) =
    let compStr =
            concatMap (\(e, s_) -> "#{" <> debug e <> "}" <> debug s_) comps
    in  "\"" <> s <> compStr <> "\""
  debug (CharLit _ c      ) = "'" <> [c] <> "'"
  debug (IntLit  _ i      ) = show i
  debug (BoolLit _ b      ) = show b
  debug (UnitLit _        ) = "()"
  debug (ListLit  _ elems ) = "[" <+> sepBy ", " (map debug elems) <+> "]"
  debug (TupleLit _ elems ) = "(" <+> sepBy ", " (map debug elems) <+> ")"
  debug (Record   _ fields) = "{" <+> sepBy ", " (map go fields) <+> "}"
    where go (name, expr) = debug name <+> "=" <+> debug expr
  debug (Project _ r f   ) = debug r <> "." <> debug f
  debug (FCall   _ f args) = "$fcall" <+> f <+> sepBy " " (map debug args)

-- Like Expr, but with type annotations on everything
-- This is the output from the typechecker (or will be, eventually)
-- There's no parameter for annotations like source spans because we don't need
-- that yet.
data ExprT n t =
    VarT n t
  | AnnT (ExprT n t) t
  | ConT n
  | HoleT n t
  -- Note that each variable bound in lambda has an annotated type
  | AbsT [(n, t)] (ExprT n t) t
  | AppT (ExprT n t) (ExprT n t) t
  | LetT [(n, ExprT n t, Maybe t)] (ExprT n t) t
  | CaseT (ExprT n t) [(Pat n, ExprT n t)] t
  | MCaseT [([Pat n], ExprT n t)] t
  | UnitLitT
  | TupleLitT [ExprT n t] t
  | ListLitT [ExprT n t] t
  | StringInterpT String [(ExprT n t, String)]
  | StringLitT String
  | CharLitT Char
  | IntLitT Int
  | BoolLitT Bool
  | RecordT [(String, ExprT n t)] t
  | ProjectT (ExprT n t) String t
  | FCallT String [ExprT n t] t
  deriving (Eq, Show, Data)

instance (Debug v, Debug t) => Debug (ExprT v t) where
  debug (VarT v _  ) = debug v
  debug (AnnT e t  ) = debug e <+> ":" <+> debug t
  debug (AppT a b _) = debug a <+> debug b
  debug (AbsT xs e _) =
    "λ" <> sepBy " " (map (debug . fst) xs) <> "." <+> debug e
  debug (ConT v   ) = debug v
  debug (HoleT s _) = "?" <> debug s
  debug (CaseT e alts _) =
    "case" <+> debug e <+> "of {" <+> sepBy "; " (map go alts) <+> "}"
    where go (pat, expr) = debug pat <+> "->" <+> debug expr
  debug (MCaseT alts _) = "mcase" <+> "{" <+> sepBy "; " (map go alts) <+> "}"
    where go (pats, expr) = sepBy " " (map debug pats) <+> "->" <+> debug expr
  debug (LetT binds e _) =
    "let"
      <+> foldl
            (\acc (x, a, t) -> case t of
              Just t' ->
                debug x <+> ":" <+> debug t' <+> "=" <+> debug a <> ";" <+> acc
              Nothing -> debug x <+> "=" <+> debug a <> ";" <+> acc
            )
            ("in" <+> debug e)
            binds
  debug (StringLitT s) = "\"" <> s <> "\""
  debug (StringInterpT s comps) =
    let compStr =
            concatMap (\(e, s_) -> "#{" <> debug e <> "}" <> debug s_) comps
    in  "\"" <> s <> compStr <> "\""
  debug (CharLitT c)         = "'" <> [c] <> "'"
  debug (IntLitT  i)         = show i
  debug (BoolLitT b)         = show b
  debug UnitLitT             = "()"
  debug (ListLitT  elems  _) = "[" <+> sepBy ", " (map debug elems) <+> "]"
  debug (TupleLitT elems  _) = "(" <+> sepBy ", " (map debug elems) <+> ")"
  debug (RecordT   fields _) = "{" <+> sepBy ", " (map go fields) <+> "}"
    where go (name, expr) = debug name <+> "=" <+> debug expr
  debug (ProjectT r f    _) = debug r <> "." <> debug f
  debug (FCallT   f args _) = "$fcall" <+> f <+> sepBy " " (map debug args)

data Pat a = VarPat a
             | WildPat
             | IntPat Int
             | CharPat Char
             | BoolPat Bool
             | UnitPat
             | TuplePat [Pat a]
             | ListPat [Pat a]
             | ConsPat a [Pat a]
             | StringPat String
             deriving (Eq, Show, Data)

instance Debug a => Debug (Pat a) where
  debug (VarPat v)       = debug v
  debug WildPat          = "_"
  debug (IntPat  i)      = show i
  debug (CharPat c)      = "'" <> [c] <> "'"
  debug (BoolPat b)      = show b
  debug UnitPat          = "()"
  debug (TuplePat args ) = "(" <> sepBy ", " (map debug args) <> ")"
  debug (ListPat  args ) = "[" <> sepBy ", " (map debug args) <> "]"
  debug (ConsPat c args) = "(" <> debug c <+> sepBy " " (map debug args) <> ")"
  debug (StringPat s   ) = "\"" <> s <> "\""

sepBy :: String -> [String] -> String
sepBy = intercalate

(<+>) :: String -> String -> String
a <+> b = a <> " " <> b

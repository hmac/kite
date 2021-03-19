module AST
  ( Expr(..)
  , ExprT(..)
  , Pat(..)
  , ConMeta(..)
  ) where

import           Data.Data                      ( Data )
import           Data.List                      ( intercalate )
import           Data.Name                      ( Name )
import           Util                           ( Debug(debug) )

-- TODO: patterns in let bindings
-- TODO: type sigs in let bindings
-- TODO: multi-definition functions in let bindings
--       (e.g. let fib 0 = 1; fib 1 = 1; fib n = ...)
data Expr n t = Var n
         | Ann (Expr n t) t
         | Con n
         | Hole n
         | Abs [n] (Expr n t)
         | App (Expr n t) (Expr n t)
         | Let [(n, Expr n t, Maybe t)] (Expr n t)
         | Case (Expr n t) [(Pat n, Expr n t)]
         | MCase [([Pat n], Expr n t)]
         | UnitLit
         | TupleLit [Expr n t]
         | ListLit [Expr n t]
         | StringInterp String [(Expr n t, String)]
         | StringLit String
         | CharLit Char
         | IntLit Int
         | BoolLit Bool
         | Record [(String, Expr n t)]
         | Project (Expr n t) String
         | FCall String [Expr n t]
         deriving (Eq, Show, Data)

instance (Debug v, Debug t) => Debug (Expr v t) where
  debug (Var v  ) = debug v
  debug (Ann e t) = debug e <+> ":" <+> debug t
  debug (App a b) = debug a <+> debug b
  debug (Abs v e) = "λ" <> debug v <> "." <+> debug e
  debug (Con  v ) = debug v
  debug (Hole s ) = "?" <> debug s
  debug (Case e alts) =
    "case" <+> debug e <+> "of {" <+> sepBy "; " (map go alts) <+> "}"
    where go (pat, expr) = debug pat <+> "->" <+> debug expr
  debug (MCase alts) = "mcase" <+> "{" <+> sepBy "; " (map go alts) <+> "}"
    where go (pats, expr) = sepBy " " (map debug pats) <+> "->" <+> debug expr
  debug (Let binds e) =
    "let"
      <+> foldl
            (\acc (x, a, t) -> case t of
              Just t' ->
                debug x <+> ":" <+> debug t' <+> "=" <+> debug a <> ";" <+> acc
              Nothing -> debug x <+> "=" <+> debug a <> ";" <+> acc
            )
            ("in" <+> debug e)
            binds
  debug (StringLit s) = "\"" <> s <> "\""
  debug (StringInterp s comps) =
    let compStr =
          concatMap (\(e, s_) -> "#{" <> debug e <> "}" <> debug s_) comps
    in  "\"" <> s <> compStr <> "\""
  debug (CharLit c)       = "'" <> [c] <> "'"
  debug (IntLit  i)       = show i
  debug (BoolLit b)       = show b
  debug UnitLit           = "()"
  debug (ListLit  elems ) = "[" <+> sepBy ", " (map debug elems) <+> "]"
  debug (TupleLit elems ) = "(" <+> sepBy ", " (map debug elems) <+> ")"
  debug (Record   fields) = "{" <+> sepBy ", " (map go fields) <+> "}"
    where go (name, expr) = debug name <+> "=" <+> debug expr
  debug (Project r f   ) = debug r <> "." <> debug f
  debug (FCall   f args) = "$fcall" <+> f <+> sepBy " " (map debug args)

-- Like Expr, but with type annotations on everything
-- This is the output from the typechecker (or will be, eventually)
data ExprT n t =
    VarT n t
  | AnnT (ExprT n t) t
  | ConT n ConMeta t
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
  debug (ConT v meta t) = debug v <+> debug meta <+> debug t
  debug (HoleT s _    ) = "?" <> debug s
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

data ConMeta = ConMeta
  { conMetaTag      :: Int
  , conMetaArity    :: Int
  , conMetaTypeName :: Name
  }
  deriving (Eq, Show, Data)

instance Debug ConMeta where
  debug (ConMeta tag arity typeName) =
    "tag="
      <>  show tag
      <+> "arity="
      <>  show arity
      <+> "typename="
      <>  show typeName

data Pat a = VarPat a
             | WildPat
             | IntPat Int
             | CharPat Char
             | BoolPat Bool
             | UnitPat
             | TuplePat [Pat a]
             | ListPat [Pat a]
             | ConsPat a (Maybe ConMeta) [Pat a]
             | StringPat String
             deriving (Eq, Show, Data)

instance Debug a => Debug (Pat a) where
  debug (VarPat v)      = debug v
  debug WildPat         = "_"
  debug (IntPat  i)     = show i
  debug (CharPat c)     = "'" <> [c] <> "'"
  debug (BoolPat b)     = show b
  debug UnitPat         = "()"
  debug (TuplePat args) = "(" <> sepBy ", " (map debug args) <> ")"
  debug (ListPat  args) = "[" <> sepBy ", " (map debug args) <> "]"
  debug (ConsPat c meta args) =
    "("
      <>  debug c
      <+> maybe mempty debug meta
      <+> sepBy " " (map debug args)
      <>  ")"
  debug (StringPat s) = "\"" <> s <> "\""

sepBy :: String -> [String] -> String
sepBy = intercalate

(<+>) :: String -> String -> String
a <+> b = a <> " " <> b

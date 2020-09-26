module Expr
  ( Expr(..)
  , Pat(..)
  )
where

import           Data.Data                      ( Data )
import           Util                           ( Debug(debug) )
import           Data.List                      ( intercalate )

data Expr n t = Var n
         | Ann (Expr n t) t
         | Con n
         | Hole n
         | Abs [n] (Expr n t)
         | App (Expr n t) (Expr n t)
         -- Note: the parser can't currently produce LetAs but the typechecker
         -- can nonetheless handle them.
         | LetA n t (Expr n t) (Expr n t)
         | Let [(n, Expr n t)] (Expr n t)
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
         -- TODO: record fields should just be strings, they have no namespace
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
  debug (LetA name ty expr body) =
    "let"
      <+> debug name
      <+> ":"
      <+> debug ty
      <+> "="
      <+> debug expr
      <+> "in"
      <+> debug body
  debug (Let binds e) =
    "let"
      <+> foldl (\acc (x, a) -> debug x <+> "=" <+> debug a <> ";" <+> acc)
                ("in" <+> debug e)
                binds
  debug (StringLit s) = "\"" <> s <> "\""
  debug (StringInterp s comps) =
    let compStr =
            concatMap (\(e, s_) -> "#{" <> debug e <> "}" <> debug s_) comps
    in  "\"" <> s <> compStr <> "\""
  debug (CharLit c      ) = "'" <> [c] <> "'"
  debug (IntLit  i      ) = show i
  debug (BoolLit b      ) = show b
  debug (UnitLit        ) = "()"
  debug (ListLit  elems ) = "[" <+> sepBy ", " (map debug elems) <+> "]"
  debug (TupleLit elems ) = "(" <+> sepBy ", " (map debug elems) <+> ")"
  debug (Record   fields) = "{" <+> sepBy ", " (map go fields) <+> "}"
    where go (name, expr) = debug name <+> "=" <+> debug expr
  debug (Project r f   ) = debug r <> "." <> debug f
  debug (FCall   f args) = "$fcall" <+> f <+> sepBy " " (map debug args)

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

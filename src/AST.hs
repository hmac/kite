module AST
  ( Expr(..)
  , ExprT(..)
  , Pat(..)
  , ConMeta(..)
  ) where

import           Data.Data                      ( Data )
import           Data.List                      ( intercalate )
import qualified Data.List.NonEmpty            as NE
import           Data.Name                      ( Name )
import           GHC.Generics                   ( Generic )
import           Util                           ( Debug(debug)
                                                , NonEmpty(..)
                                                )

-- TODO: patterns in let bindings
-- TODO: type sigs in let bindings
-- TODO: multi-definition functions in let bindings
--       (e.g. let fib 0 = 1; fib 1 = 1; fib n = ...)
data Expr n t = Var n
         | Ann (Expr n t) t
         | Con n
         | Hole n
         | Abs (NonEmpty n) (Expr n t)
         | App (Expr n t) (Expr n t)
         | Let [(n, Expr n t, Maybe t)] (Expr n t)
         | Case (Expr n t) [(Pat n, Expr n t)]
         -- TODO: these should probably be NonEmpty
         | MCase [([Pat n], Expr n t)]
         | UnitLit
         | TupleLit [Expr n t]
         | ListLit [Expr n t]
         | StringInterp String (NonEmpty (Expr n t, String))
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
-- We store types on all constructors, even ones like Unit for which the type is obvious.
-- This makes it easier to write generic functions like 'Syn.Typed.typeOf'.
data ExprT n t =
    VarT t n
  -- First 't' is type cache, second 't' is user-supplied annotation
  | AnnT t (ExprT n t) t
  | ConT t n ConMeta
  | HoleT t n
  -- Note that each variable bound in lambda has an annotated type
  | AbsT t (NonEmpty (n, t)) (ExprT n t)
  -- Binding of an implicit argument.
  -- This construct doesn't exist in the surface syntax - it is constructed by the typechecker when
  -- elaborating implicit function types.
  | IAbsT t n t (ExprT n t)
  | AppT t (ExprT n t) (ExprT n t)
  -- Application of an implicit argument.
  -- Like 'IAbstT', this doesn't exist in the surface syntax.
  -- TODO: can we remove this?
  | IAppT t (ExprT n t) (ExprT n t)
  | LetT t [(n, ExprT n t, Maybe t)] (ExprT n t)
  | CaseT t (ExprT n t) [(Pat n, ExprT n t)]
  -- TODO: store types on patterns, similar to what we do with abstractions
  | MCaseT t [([Pat n], ExprT n t)]
  | UnitLitT t
  | TupleLitT t [ExprT n t]
  | ListLitT t [ExprT n t]
  | StringInterpT t String (NonEmpty (ExprT n t, String))
  | StringLitT t String
  | CharLitT t Char
  | IntLitT t Int
  | BoolLitT t Bool
  | RecordT t [(String, ExprT n t)]
  | ProjectT t (ExprT n t) String
  | FCallT t String [ExprT n t]
  deriving (Eq, Show, Data, Generic)

instance (Debug v, Debug t) => Debug (ExprT v t) where
  debug (VarT _ v   ) = debug v
  debug (AnnT  _ e t) = debug e <+> ":" <+> debug t
  debug (AppT  _ a b) = debug a <+> debug b
  debug (IAppT _ a b) = debug a <+> debug b
  debug (AbsT _ xs e) =
    "λ" <> sepBy " " (map (debug . fst) (NE.toList xs)) <> "." <+> debug e
  debug (IAbsT _ n _ e) = "λ" <> debug n <> "=>" <+> debug e
  debug (ConT t v meta) = debug v <+> debug meta <+> debug t
  debug (HoleT _ s    ) = "?" <> debug s
  debug (CaseT _ e alts) =
    "case" <+> debug e <+> "of {" <+> sepBy "; " (map go alts) <+> "}"
    where go (pat, expr) = debug pat <+> "->" <+> debug expr
  debug (MCaseT _ alts) = "mcase" <+> "{" <+> sepBy "; " (map go alts) <+> "}"
    where go (pats, expr) = sepBy " " (map debug pats) <+> "->" <+> debug expr
  debug (LetT _ binds e) =
    "let"
      <+> foldl
            (\acc (x, a, t) -> case t of
              Just t' ->
                debug x <+> ":" <+> debug t' <+> "=" <+> debug a <> ";" <+> acc
              Nothing -> debug x <+> "=" <+> debug a <> ";" <+> acc
            )
            ("in" <+> debug e)
            binds
  debug (StringLitT _ s) = "\"" <> s <> "\""
  debug (StringInterpT _ s comps) =
    let compStr =
          concatMap (\(e, s_) -> "#{" <> debug e <> "}" <> debug s_) comps
    in  "\"" <> s <> compStr <> "\""
  debug (CharLitT _ c      ) = "'" <> [c] <> "'"
  debug (IntLitT  _ i      ) = show i
  debug (BoolLitT _ b      ) = show b
  debug (UnitLitT _        ) = "()"
  debug (ListLitT  _ elems ) = "[" <+> sepBy ", " (map debug elems) <+> "]"
  debug (TupleLitT _ elems ) = "(" <+> sepBy ", " (map debug elems) <+> ")"
  debug (RecordT   _ fields) = "{" <+> sepBy ", " (map go fields) <+> "}"
    where go (name, expr) = debug name <+> "=" <+> debug expr
  debug (ProjectT _ r f   ) = debug r <> "." <> debug f
  debug (FCallT   _ f args) = "$fcall" <+> f <+> sepBy " " (map debug args)

data ConMeta = ConMeta
  { conMetaTag      :: Int
  , conMetaArity    :: Int
  , conMetaTypeName :: Name
  }
  deriving (Eq, Show, Data, Generic)

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
             deriving (Eq, Show, Data, Generic)

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

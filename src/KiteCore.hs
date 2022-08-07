module KiteCore where

-- KiteCore is a simplified version of Kite.
-- All lambdas are lifted to top-level functions.
-- It removes nested pattern matches, string interpolation, multi-case, list
-- literals, and a bunch of other stuff, keeping just the essentials.
-- It is untyped.
-- It also establishes a clear, strict evaluation order.
-- It should be easier to compile KiteCore to a sequence of instructions.
--
-- > p    = fn, ...                                 program
-- > fn   = fn <name>(x, ...) { e }                 function
-- > e    = a                                       atomic expression
-- >      | let x = e1 in e2                        let
-- >      | case x {p1 -> e1, ...}                  case
-- >      | "<string>"                              string literal
-- >      | '<char'                                 char literal
-- >      | <int>                                   int literal
-- >      | <bool>                                  bool literal
-- >
-- > a    = x                                       variable
-- >      | C a ...                                 constructor application
-- >      | a1 a2                                   application
-- >
-- > p    = C x1 x2 ...                             constructor pattern
-- >      | x1                                      variable pattern

-- | A KiteCore expression.
-- 'n' is the type of variable names.
data Exp n
  = Atom (AtomicExp n)
  | Let n (Exp n) (Exp n)
  | Case n [(Pat n, Exp n)]
  | StringLit String
  | CharLit Char
  | IntLit Int
  | BoolLit Bool

-- | A atomic KiteCore expression. "Atomic" isn't very well defined here, but it
-- basically ensures that there's a clear evaluation order by not allowing lets
-- or cases inside applications.
-- Constructors are named by their integer tag.
data AtomicExp n
  = Var n
  | Ctor Int [AtomicExp n]
  | App (AtomicExp n) (AtomicExp n)

-- | A pattern.
data Pat n
  = CtorPat Int [n]
  | VarPat n

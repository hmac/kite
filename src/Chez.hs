module Chez where

import           Data.Text                      ( Text )

data Def = Def Text SExpr
         | DefRecord Text [Text]
         | DefFunc Text [Text] SExpr
  deriving (Eq, Show)

data SExpr = Lit Lit
           -- This is a syntactic list, not the data type list.
           -- Kite lists are compiled to Chez Scheme vectors (Vec).
           | List [SExpr]
           | Vec [SExpr]
           | Var Text
           | App SExpr [SExpr]
           | Abs [Text] SExpr
           | Let [(Text, SExpr)] SExpr
           | Cond [(SExpr, SExpr)]
           | Quote SExpr
  deriving (Eq, Show)

data Lit = Int Int
         | Char Char
         | String Text
         | Bool Bool
         | Unit
  deriving (Eq, Show)

-- (define sign
--  (lambda (n)
--    (cond
--      [(< n 0) -1]
--      [(> n 0) +1]
--      [(= n 0) 0])))
example :: Def
example = Def "sign" $ Abs ["n"] $ Cond
  [ (App (Var "<") [Var "n", Lit (Int 0)], Lit (Int (-1)))
  , (App (Var ">") [Var "n", Lit (Int 0)], Lit (Int 1))
  , (App (Var "=") [Var "n", Lit (Int 0)], Lit (Int 0))
  ]

true :: SExpr
true = Var "#t"

false :: SExpr
false = Var "#f"

null :: SExpr
null = Quote (List [])

-- Sequence a series of expressions, returning the result of the last one
begin :: [SExpr] -> SExpr
begin = App (Var "begin")

-- Raise a non-continuable exception with the given message.
-- This will (or should) halt the program.
panic :: Text -> SExpr
panic msg = App (Var "raise") [App (Var "condition") [App (Var "make-violation") [], App (Var "make-message-condition") [Lit (String msg)]]]

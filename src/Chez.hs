module Chez where

import           Data.Text                      ( Text )

data Def = Def Text SExpr
         | DefRecord Text [Text]
  deriving (Eq, Show)

data SExpr = Lit Lit
           | List [SExpr]
           | Var Text
           | App SExpr [SExpr]
           | Abs [Text] SExpr
           | Let [(Text, SExpr)] SExpr
           | If SExpr SExpr SExpr
           | Cond [(SExpr, SExpr)]
  deriving (Eq, Show)

data Lit = Int Int
         | Char Char
         | String String
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

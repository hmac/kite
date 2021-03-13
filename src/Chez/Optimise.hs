module Chez.Optimise where

import           Chez                           ( SExpr(..) )

-- Remove empty letrecs
-- (currently unused)
optimise :: SExpr -> SExpr
optimise = \case
  Lit  l        -> Lit l
  List xs       -> List xs
  Vec  xs       -> Vec xs
  Var  v        -> Var v
  App f    args -> App f args
  Abs args body -> Abs args body
  Let []   e    -> e
  Let xs   e    -> Let xs e
  Cond  alts    -> Cond alts
  Quote e       -> Quote e

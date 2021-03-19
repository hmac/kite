module Test.QQ where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Syn.Parse                      ( pExpr
                                                , pFun
                                                , pModule
                                                , pType
                                                , spaceConsumerN
                                                )
import           Text.Megaparsec                ( eof
                                                , errorBundlePretty
                                                , parse
                                                )

-- A QuasiQuoter for Kite surface syntax
-- [syn|x -> x] ==> MCase [([VarPat x], Var x)]
syn :: QuasiQuoter
syn = QuasiQuoter { quoteExp  = f
                  , quotePat  = undefined
                  , quoteType = undefined
                  , quoteDec  = undefined
                  }
 where
  f :: String -> Q Exp
  f s = case parse (pExpr <* eof) "" s of
    Left  err  -> error (errorBundlePretty err)
    Right expr -> dataToExpQ (const Nothing) expr

-- A QuasiQuoter for types
-- [ty|forall a. a -> [a]] ==> Forall [a] (Fn (Var a) (TyApp List (Var a)))
typ :: QuasiQuoter
typ = QuasiQuoter { quoteExp  = f
                  , quotePat  = undefined
                  , quoteType = undefined
                  , quoteDec  = undefined
                  }
 where
  f :: String -> Q Exp
  f s = case parse (pType <* eof) "" s of
    Left  err -> error (errorBundlePretty err)
    Right t   -> dataToExpQ (const Nothing) t

-- A QuasiQuoter for functions
-- [fn|inc : Int -> Int
--     inc = x -> x + 1]] ==> Fun { funName = "inc", ... }
fn :: QuasiQuoter
fn = QuasiQuoter { quoteExp  = f
                 , quotePat  = undefined
                 , quoteType = undefined
                 , quoteDec  = undefined
                 }
 where
  f :: String -> Q Exp
  f s = case parse (spaceConsumerN *> pFun <* eof) "" s of
    Left  err -> error (errorBundlePretty err)
    Right t   -> dataToExpQ (const Nothing) t

-- A QuasiQuoter for module
mod :: QuasiQuoter
mod = QuasiQuoter { quoteExp  = f
                  , quotePat  = undefined
                  , quoteType = undefined
                  , quoteDec  = undefined
                  }
 where
  f :: String -> Q Exp
  f s = case parse (spaceConsumerN *> pModule <* eof) "" s of
    Left  err -> error (errorBundlePretty err)
    Right t   -> dataToExpQ (const Nothing) t

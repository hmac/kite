module Type.Function where

-- Typechecking of function defintions

import           Control.Monad                  ( void )
import           Type                           ( Type
                                                , Exp
                                                , Ctx
                                                , check
                                                , TypeM
                                                , wellFormedType
                                                )

checkFun :: Ctx -> Type -> Exp -> TypeM ()
checkFun ctx funTy body = do
  -- check the type is well-formed
  void $ wellFormedType ctx funTy
  -- check the body of the function
  void $ check ctx body funTy

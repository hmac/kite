module Type.Function where

-- Typechecking of function defintions

import           Data.Foldable                  ( foldlM )
import           Control.Monad                  ( void )
import           Type                           ( Type
                                                , Exp
                                                , Ctx
                                                , check
                                                , TypeM
                                                , wellFormedType
                                                , unfoldFn
                                                , foldFn
                                                , throwError
                                                , Error(..)
                                                , checkPattern
                                                , Pattern
                                                )

checkFun :: Ctx -> Type -> [([Pattern], Exp)] -> TypeM ()
checkFun ctx funTy equations = do
  -- check the type is well-formed
  void $ wellFormedType ctx funTy
  -- check each equation against the type
  mapM_ (checkEquation ctx funTy) equations

checkEquation :: Ctx -> Type -> ([Pattern], Exp) -> TypeM ()
checkEquation ctx funTy (pats, rhs) = do
  -- unfold the (function) type
  let (argTys, lastTy) = unfoldFn funTy
  -- If there are fewer arg types than patterns, throw an error
  if length argTys < length pats
    then throwError TooManyPatterns
    else do
      -- check each pat against the corresponding arg type, accumulating a new
      -- context
      let patsWithTys = zip pats argTys
      ctx' <- foldlM (\ctx_ (pat, ty) -> checkPattern ctx_ pat ty)
                     ctx
                     patsWithTys
      -- check the rhs against the remaining result type, with the context
      let resultTy = foldFn (drop (length pats) argTys) lastTy
      void $ check ctx' rhs resultTy

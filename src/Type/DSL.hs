-- A little DSL for writing types without all the 'TOther' wrapping
-- The primed functions return 'Type'', which is useful if you want to then use 'tapp'.
module Type.DSL where

import           Data.Name                      ( Name )
import           Type.Type                      ( E
                                                , Type(..)
                                                , Type'(..)
                                                , U
                                                )

fn :: Type -> Type -> Type
fn a b = TOther $ Fn a b

fn' :: Type -> Type -> Type'
fn' = Fn

ifn :: Type -> Type -> Type
ifn a b = TOther $ IFn a b

ifn' :: Type -> Type -> Type'
ifn' = IFn

forAll :: U -> Type -> Type
forAll u t = TOther $ Forall u t

forAll' :: U -> Type -> Type'
forAll' = Forall

e_ :: E -> Type
e_ = TOther . EType

e_' :: E -> Type'
e_' = EType

u_ :: U -> Type
u_ = TOther . UType

u_' :: U -> Type'
u_' = UType

trecord :: [(String, Type)] -> Type
trecord = TOther . TRecord

trecord' :: [(String, Type)] -> Type'
trecord' = TRecord

ttuple :: [Type] -> Type
ttuple = TOther . ttuple'

ttuple' :: [Type] -> Type'
ttuple' = TTuple

tcon :: Name -> [Type] -> Type
tcon = TCon

tapp :: Type' -> [Type] -> Type
tapp = TApp

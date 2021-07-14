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

tcon :: Name -> [Type] -> Type
tcon n ts = TCon n ts

tapp :: Type' -> [Type] -> Type
tapp f ts = TApp f ts

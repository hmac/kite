{-# LANGUAGE QuasiQuotes #-}
module Test.Type.Module where

import           Prelude                 hiding ( either
                                                , maybe
                                                , mod
                                                )
import           Test.Hspec
import           Type                           ( defaultTypeEnv
                                                , runTypecheckM
                                                )
import           Type.Module                    ( checkModule )
import           Type.Print                     ( printLocatedError )
import           Type.Type                      ( Ctx
                                                , CtxElem(..)
                                                , Type(..)
                                                , TypeCtx
                                                , U(..)
                                                , V(..)
                                                )

import           Canonicalise                   ( canonicaliseModule )
import           Test.QQ

import qualified Syn

test :: Spec
test = do
  describe "typing functions" $ do
    it "a simple function" $ checks [fn|
f : Bool -> Bool
f = x -> case x of
           True -> True
           False -> False|]
    it "a function with pattern matching" $ checks [fn|
f : Bool -> Bool
f = True -> True
    False -> False|]
    it "list patterns" $ checks [fn|
f : [a] -> Bool
f = [] -> False
    (x :: xs) -> True|]
    it "a recursive function" $ checks [fn|
f : [a] -> Bool
f = [] -> True
    (x :: xs) -> f xs|]
    it "tuple patterns" $ checks [fn|
f : (Bool, Int) -> Int
f = (True, y) -> y
    (x, 0) -> 0|]
    it "constructors with arguments" $ checks [fn|
f : Pair Bool Nat -> Bool
f = (MkPair x Zero) -> x
    _               -> False|]
    it "Either Bool Nat" $ checks [fn|
f : Either Bool Nat -> Nat
f = (Left False) -> Zero
    (Left _)     -> Suc Zero
    (Right n)    -> n|]
    it "Maybe Bool" $ checks [fn|
f : Maybe Bool -> Bool
f = (Just b) -> b
    Nothing  -> False|]
    it "A foreign call" $ checks [fn|
f : String -> ()
f = s -> $fcall putStrLn s|]
  describe "expected type failures" $ do
    -- TODO: check the error message matches what we expect
    it "mismatched constructors in pattern" $ fails [fn|
f : Bool -> Bool
f = True -> True
    Zero -> False|]
    it "type mismatch between list pattern and boolean" $ fails [fn|
f : [a] -> Bool
f = [] -> False
    x -> x|]
    it "type mismatch in tuple pattern" $ fails [fn|
f : (Bool, a) -> a
f = (True, y) -> y
    (Zero, x) -> x|]
    it "type mismatch between function and annotation" $ fails [fn|
id : a -> a
id = x -> 5|]
    it "different numbers of patterns in equations" $ fails [fn|
f : Bool -> Bool
f = True False -> True|]
  describe "typing simple modules" $ do
    it "five : Int; five = 5" $ checksModule [mod|
module Foo
five : Int
five = 5|]
    it "const : a -> b -> a; const x y = x" $ checksModule [mod|
module Foo
const : a -> b -> a
const = x y -> x|]
    it "fromMaybe" $ checksModule [mod|
module Foo
type Maybe a = Just a | Nothing
fromMaybe : Maybe a -> a -> a
fromMaybe = (Just x) _ -> x
            Nothing  y -> y|]
  describe "expected typing failures" $ do
    it "id : a -> a; id x = 5" $ failsModule [mod|
module Foo
id : a -> a
id = x -> 5|]
    it "const : a -> b -> a; const x y = y" $ failsModule [mod|
module Foo
const : a -> b -> a
const = x y -> y|]

ctx :: (TypeCtx, Ctx)
ctx =
  let
    nat = TCon (qq "Nat") []
    wrap a = TCon (qq "Wrap") [a]
    pair a b = TCon (qq "Pair") [a, b]
    either a b = TCon (qq "Either") [a, b]
    maybe a = TCon (qq "Maybe") [a]
    termCtx =
      [ V (Free (qq "Zero")) nat
      , V (Free (qq "Suc"))  (Fn nat nat)
      , V (Free (qq "MkWrap"))
          (let a = U 0 "a" in Forall a $ Fn (UType a) (wrap (UType a)))
      , V
        (Free (qq "MkPair"))
        (let a = U 1 "a"
             b = U 2 "b"
         in  Forall a $ Forall b $ Fn
               (UType a)
               (Fn (UType b) (pair (UType a) (UType b)))
        )
      , V
        (Free (qq "Left"))
        (let a = U 1 "a"
             b = U 2 "b"
         in  Forall a $ Forall b $ Fn (UType a) (either (UType a) (UType b))
        )
      , V
        (Free (qq "Right"))
        (let a = U 1 "a"
             b = U 2 "b"
         in  Forall a $ Forall b $ Fn (UType b) (either (UType a) (UType b))
        )
      , V (Free (qq "Nothing")) (let a = U 1 "a" in Forall a (maybe (UType a)))
      , V (Free (qq "Just"))
          (let a = U 1 "a" in Forall a (Fn (UType a) (maybe (UType a))))
      ]
    typeCtx =
      map (, ()) [qq "Nat", qq "Wrap", qq "Pair", qq "Either", qq "Maybe"]
  in
    (typeCtx, termCtx)

checks :: Syn.Fun Syn.Syn -> Expectation
checks = checksModule . mkModule

fails :: Syn.Fun Syn.Syn -> Expectation
fails = failsModule . mkModule

mkModule :: Syn.Fun Syn.Syn -> Syn.Module
mkModule fun = Syn.Module { Syn.moduleName     = "qq.QQ"
                          , Syn.moduleImports  = mempty
                          , Syn.moduleExports  = mempty
                          , Syn.moduleDecls    = [Syn.FunDecl fun]
                          , Syn.moduleMetadata = mempty
                          }

checksModule :: Syn.Module -> Expectation
checksModule modul = do
  let (typeCtx, termCtx) = ctx
      r = checkModule (typeCtx, termCtx, mempty) (canonicaliseModule modul)
        >> pure ()
  case runTypecheckM defaultTypeEnv r of
    Left  err -> expectationFailure $ show (printLocatedError err)
    Right ()  -> pure ()

failsModule :: Syn.Module -> Expectation
failsModule modul = do
  let (typeCtx, termCtx) = ctx
      r = checkModule (typeCtx, termCtx, mempty) (canonicaliseModule modul)
        >> pure ()
  case runTypecheckM defaultTypeEnv r of
    Left _ -> pure ()
    Right () ->
      expectationFailure "expected typechecking to fail, but it succeeded"

# Deriving

Haskell supports typeclass deriving, where you can write this:

```haskell
data Flip = Heads | Tails deriving Eq
```

and the compiler will generate something like:

```haskell
data Flip = Heads | Tails deriving Eq

instance Eq Flip where
  Heads == Heads = True
  Tails == Tails = True
  _     == _     = False
```

This is useful because these typeclass instances are often obvious and dull to
write. We want to support a similar thing in Lam, but how?

Lam's typeclasses look like this:

```haskell
type alias Eq a = { eq : a -> a -> Bool }
```

So then the following Lam code

```haskell
type Flip = Heads | Tails deriving Eq
```

Should generate something like:

```haskell
eqFlip : Eq Flip
eqFlip = { eq = _eqFlip }

_eqFlip : Flip -> Flip -> Bool
_eqFlip Heads Heads = True
_eqFlip Tails Tails = True
_eqFlip _     _     = False
```

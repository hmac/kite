# Type aliases

## Motivation

Lam supports type aliases, which look like this:

```haskell
type alias MyAlias a b = (a, List b)
```

This defines a new alias called `MyAlias` which takes two type parameters `a`
and `b`, and is identical to the type `(a, List b)`. This is useful in many
scenarios, but in particular it can make it a lot easier to work with Lam's
typeclass-dictionary things.

Currently we define the `Functor` typeclass like this:

```haskell
type Functor f = Functor { map : (a -> b) -> f a -> f b }

map : Functor f -> (a -> b) -> f a -> f b
map (Functor d) = d.map
```

For any (higher kinded) type `f`, `Functor f` is a data type containing one
field, which is a record with a single field `map`. To the map method of a
Functor instance of this type, you first need to unwrap the data type to get at
the record, and then you can access its field. We provide the `map` function as
a convenience wrapper to do exactly this.

Defining a typeclass instance is just a case of constructing a value of the
type:

```haskell
functorList : Functor []
functorList = Functor { map = \f xs -> ... }
```

And we can use it like this:

```haskell
map functorList (\x -> x * x) [1,2,3]
```

If we had type aliases, we could instead define `Functor` as follows:

```haskell
type alias Functor f = { map : (a -> b) -> f a -> f b }

map : Functor f -> (a -> b) -> f a -> f b
map d = d.map
```

We no longer need to wrap and unwrap the type, and we can just directly extract
the `map` field from the type. The advantage of this approach becomes more
pronounced when we consider superclasses.

The `Ord` typeclass is defined as follows:

```haskell
type Ord a = Ord { eq : Eq a, compare : a -> a -> Ordering }
```

where `Eq` is another typeclass. This is how we encode superclass constraints:
any type that is an instance of `Ord` must also be an instance of `Eq`, because
its `Ord` instance must contain an `Eq` instance.

Without type aliases, we have to do a bit of a dance to access the `Eq` methods:

```haskell
ordEq : Ord a -> a -> a -> Bool
ordEq (Ord d) x y = let (Eq r) = d.eq in r.eq x y

-- Or, with the following convenience wrapper...
eq : Eq a -> a -> a -> Bool
eq (Eq d) = d.eq

-- ...we can make it slightly nicer
ordEq' : Ord a -> a -> a -> Bool
ordEq' (Ord d) x y = eq d.eq x y
```

With type aliases, we can clean this up a bit:

```haskell
type alias Ord a = { eq : Eq a, compare : a -> a -> Ordering }

ordEq : Ord a -> a -> Bool
ordEq ord x y = ord.eq.eq x y
```

With some support for merging record types, we could in the future have

```haskell
type alias Ord a = { Eq a, compare : a -> a -> Ordering }
```

where the first field of the record says "merge all the fields of `Eq a` with
this record".

## Problem

The difficulty with implementing type aliases boils down to the fact that it
binds new type variables. A type aliases is effectively a name for a universally
quantified type (the compiler calls these `Scheme`s).

```
type alias Foo a = (a, a)
-- is equivalent to (with made-up syntax)
Foo = forall a. (a, a)
```

We want to convert all type aliases to their actual types before type checking,
as this means that the type checker doesn't need to know about them and we're
guaranteed that they behave exactly like their corresponding types. The
difficulty is that to properly substitute a type alias for its type you need it
to be fully applied.

e.g. for the `Foo` alias above, we need to scan all our types looking for
patterns like this:

```
TyApp (TyAlias "Foo" (Forall ["a"] (TyTuple ["a", "a"]))) t
```

We then need to reduce this expression by unifying the variable `a` with the
type `t`, and substituting `t` into the body of the `Forall`:

```
   [t/a] (TyTuple ["a", "a"])
=> TyTuple [t, t]
```

This isn't an impossible task, but consider that the type alias may be buried
deep inside a type somewhere, and that it may itself expand to _another_ alias,
which will have to be resolved. It starts to look like types need to be
evaluated to a normal form - and so we need to make sure that this evaluation
terminates! This means we should disallow mutually recursive aliases like this:

```
type alias Foo = Bar
type alias Bar = Foo
```

I'm not yet sure what the nicest solution is, so I've written this document in
an attempt to crystallise the problem.

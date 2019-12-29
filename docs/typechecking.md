Typechecking
============

Lam's type system is pretty much identical to Haskell 98, but written from
scratch for simplicity, clarity and (eventually) speed. GHC has many complex
type extensions that Lam intentionally avoids.

The original design of the typechecker was built around a clone of the
implementation in Typing Haskell in Haskell by Mark P Jones (from here on,
THIH). We take the surface syntax `Syn` and desugar it to a simpler language
called `Core`. `Core` is then translated into a set of data structures
understood by THIH, which spits out either an error or a set of type assignments
for every definition in the program. We ignore the type assignments and just use
the absence of an error as a gate to the next stage of compilation.

In the next stage, we again take the surface syntax and compile it to
successively smaller languages (ELC and LC) until we have something that we can
evaluate.

This scheme worked well until we came to implement typeclasses and typeclass
instances. Typeclass instances are typically translated to "dictionaries" -
record types containing a field for each method in the typeclass. Class
constraints in functions are translated to plain arguments of the corresponding
record type. For example:

```haskell
class Eq a where
  eq : a -> a -> Bool

instance Eq Int where
  eq = primIntEq

nub : Eq a => [a] -> [a]
nub = ...

foo : Eq a => [a] -> [a]
foo xs = nub (reverse xs)

bar : [Int] -> [Int]
bar xs = foo xs
```

would be translated to

```haskell
data Eq a = Eq { _eq : a -> a -> Bool }

eq : Eq a -> a -> a -> Bool
eq d x y = _eq d x y

$eqInt = Eq { eq = primIntEq }

nub : Eq a -> [a] -> [a]
nub = ...

foo : Eq a -> [a] -> [a]
foo d xs = nub d (reverse xs)

bar : [Int] -> [Int]
bar xs = foo $eqInt xs
```

For this translation to work, it's necessary to know the concrete type of every
variable in every class constraint. For the translation of `bar` above, we must
know that the `Eq a` in `foo` is instantiated to `Eq Int` in order to find the
corresponding dictionary `$eqInt` and insert it.

The key point is that _inferred type information must be present for
compilation_. We cannot typecheck and then throw away the results. This
motivates us to use the same program representation across typechecking and
compilation. The natural conclusion of this train of thought is the following
architecture:

```
[PARSE]
   v        Surface syntax
[DESUGAR]
   v        Core, with partial type information
[TYPECHECK]
   v        Core, with full type information
[COMPILE]
            Lambda calculus
```

We start by desugaring the surface syntax to a core AST which is amenable both
to typechecking and compilation. The only point of contention here is how to
represent let bindings and pattern matches - the current approaches differ. The
core AST will have type annotations on lambda bindings and polymorphic function
application, but in most cases they will be marked as "unknown", either with
type holes or explicit variables.

The type checker will attempt to infer all unknown types, which wil be filled
in. The output is a fully annotated Core AST. This can then be compiled,
complete with typeclass dictionary insertion.

Typechecking as constraint solving
----------------------------------

As well as this new architecture, we can also redesign the typechecker itself to
use a very different approach based on explicit constraint generation followed
by constraint solving. This is what GHC has evolved to do, and has some nice
benefits. Namely, that constraint generation involves many cases but all are
_simple_, and constraint solving is complex but involves _few cases_.

The idea is that for each term in the language you can generate a set of
constraints which must be satisfied in order to determine a unique, correct type
for the term. For example, consider the following program fragment:

```haskell
reverse : [a] -> [a]
and : [Bool] -> Bool

foo xs = (reverse xs, and xs)
```

From this fragment we can generate the following constraints:
```
-- from 'reverse xs'
xs : ⍺
reverse : [β] -> [β]
⍺ ~ [β]
-- from 'and xs'
⍺ ~ [Bool]
```

The first constraint is generated from the argument `xs`, which has an unknown
type (represented by the variable ⍺). The second constraint is generated from
the type signature for `reverse`, with the type variable `a` instantiated to a
fresh variable β. The third constraint models the expectation that the type of
the argument given to reverse must match the first argument in its type
signature. The fourth constraint does the same for `and xs`, requiring that its
input type (⍺, the as-yet-unknown type of `xs`) be equal to `[Bool]`.

Given this set of constraints, we can attempt to simplify/solve them.
Informally, this goes as follows:

1. `⍺ ~ [Bool]`, therefore ⍺ is `[Bool]`.
2. Applying (1) to `⍺ ~ [β]`, we get `[Bool] ~ [β]`
3. From (3), we conclude `β ~ Bool`, therefore β is `Bool`.

This gives a resulting substitution `⍺ := [Bool], β := Bool`.

We can then apply the substitution to the original AST to produce a fully
typechecked and type annotated AST.

I'm not yet sure whether this change is orthogonal to the architecture change or
not. Both are inspired by recent GHC designs but it may be easier to separate
them out to avoid doing too much at once. If we can keep THIH in the new
architecture then we should do that, if only temporarily.

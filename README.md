# Kite

> A small, strict, modern functional programming language.

Kite is a statically typed, strict, purely functional programming language with full type inference.
It's the core parts of Haskell with better ergonomics, an interpreter, native JS
support and much better tooling.

Kite aims to be good for web applications and scripts. For teams building big
Rails apps that are struggling with stability and productivity as their codebase
grows, Kite should provide a compelling alternative.

Kite is also intended to be a gateway to Haskell. Migration from Kite to
Haskell should be feasible. Kite is _not_ intended to replace Haskell. Its type
system is much less powerful than GHC's and its performance will be much lower.
If you're happy with Haskell then please stick with it!

An example Kite program:

```haskell
module Main

from std import Data.String (toUpper)

type User = User { name : String, age : Int }

main : IO ()
main = do
  name <- getLine
  age <- map read getLine
  let user = User { name, age }
  greet name

greet : User -> IO ()
greet = User { name } -> putLine "Hello #{toUpper name}!"
```

A few things immediately jump out:
- Similar syntax to Haskell, with some minor differences
  - single colons for type annotations
  - `type` instead of `data` for declaring new data types
- Ruby-esque string interpolation
- Imports specify their package
- [Multi-case](https://github.com/hmac/kite/blob/main/README.md#multi-case-expressions)

Kite has many other features which are described below.

## Status

Kite is in early development and most of its features don't exist yet. Here's
the current state of progress:

- [x] Basic Haskell-style syntax
- [x] Pattern matching
- [x] Multi-case
- [x] String interpolation
- [x] Source code formatting
- [x] Interpreter
- [x] Typed holes
- [ ] LSP
- [ ] Implicits
- [ ] Standard library
- [ ] Totality checker
- [ ] Safety checker
- [ ] Errors & warnings
- [ ] Data-generic programming support
- [x] Anonymous records
- [ ] Markdown support in comments
- [ ] Multiline strings
- [ ] Overloaded string literals
- [ ] Doctests
- [ ] Automatic test discovery
- [-] REPL
- [ ] Go backend
- [ ] JS backend
- [-] Packaging
- [ ] IO and runtime support

## Type system

Kite's type system is basically Hindley-Milner plus extensible records. In this
sense it's quite similar to Elm. However Kite's type system provides some
additional tools, including typed holes and implicit arguments.

### Typed Holes

Kite supports typed holes in terms. With this code:
```haskell
foo : a -> Maybe a -> a
foo = x m -> ?1
```

You'll get an error like this:
```
Hole:
       ?1 : a
In:    foo = x m -> ?1
Scope: x : a
       m : Maybe a
Fits:  x
```

All holes begin with a question mark. The compiler will attempt to infer the
type of `?1` and `?2`. Holes in types must be filled for typechecking to
succeed, but holes in terms may be left (and will generate a warning). If the
program encounters a term hole at runtime it will throw an error. This is quite
useful if you have half a program written and just want to try it out quickly.

### Case splitting

Holes give a useful marker from which we can perform other interactive steps.
When Kite gains LSP support, this will include case splitting on variables near
holes, much like Agda and Idris.

```haskell
foo : a -> Maybe a -> a
foo = x m -> ?1

-- case-split ?1 m

foo : a -> Maybe a -> a
foo = x (Just m1) -> ?1
      x Nothing   -> ?2
```

Case split works by looking at they type of the variable and the constructors of
that type. For each constructor it generates a new definition, creating fresh
variables for constructor arguments. The RHS must be a single hole for case
split to be used (otherwise we would clobber whatever definition was there).

### Totality

Kite has a totality checker that will mark functions as total if it can be sure
that they are. A function is considered total if it:
- isn't recursive
- doesn't use IO or FFI
- doesn't call error
- doesn't call a non-total function

This is quite conservative, as in reality many recursive functions are total
(provided they recurse on structurally smaller arguments). If it's not a huge
pain I might make the totality checker smarter in this regard, but it's not a
definite feature.

You're not forced to make any of your functions total, but tooling can inspect
this property and in the future the type system may be able to specify it as a
constraint, e.g.

```haskell
totalMap : Functor f => Total (a -> b) -> f a -> f b
```

### Safety

Kite will also have a concept of module safety and a safety checker. This is
similar to [Safe Haskell](https://wiki.haskell.org/Safe_Haskell). A safe module
is guaranteed not to perform IO or FFI and therefore poses no security risk. The
worst it can do is put your program in loop or throw an error. Therefore you can
depend on a safe package without worrying about it stealing your secrets in
production, and the automated update of safe dependencies poses no security risk
either. A module is safe if none of the functions defined in it use IO or FFI.

### Implicits

Kite doesn't support typeclasses in the traditional sense. Instead, it takes the
[Scrap Your Typeclasses](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html)
philosophy and extends it with some extra tooling. Typeclasses in Kite are just
record types, like this:
```haskell
type Eq a = Eq { eq : a -> a -> Bool }

eq : Eq a -> a -> a -> Bool
eq = (Eq r) -> r.eq
```
Typeclass instances are records:
```haskell
eqBool = Eq { eq = \b1 b2 -> ... }
```
Functions with typeclass constraints just take records as extra arguments:
```haskell
-- True if the list contains the given element
contains : Eq a -> a -> [a] -> True
contains =
  _ _ []    -> False
  r x y::ys -> case eq r x y of
                 True -> True
                 False -> contains r x ys
```

When calling `contains` on a concrete type, we pass in a concrete typeclass
instance:

```haskell
contains eqBool True [True, False]
```

## Implicit Arguments

It's quite a drag to write these typeclass instances everywhere, so Kite supports
implicit arguments to automate this. You can declare a function with an implicit
argument as follows:
```haskell
eq : Eq a => -> a -> a -> Bool
eq = Eq r => r.eq
```

The syntax `=>` is an implicit function arrow. The argument on the left of a
`=>` is an implicit argument, which is not supplied when calling the function.
The argument on the right is treated normally. At the call site, Kite
will attempt to infer a value for the argument.

For example, in the following call to `eq`:

```haskell
eq True True
```

Kite will search the current scope for a value of type `Eq Bool`. If it finds
_exactly one_ match, it will insert it into the call site. If it finds zero or
more than one match, it will report an error.

This provides most of the ergonomics of Haskell typeclasses with a small amount
of additional syntax, and avoids any new namespaces or complex resolution logic.

### Typeclass deriving

Kite will be able to automatically generate typeclass instances for some common typeclasses:
- Eq, Show, Debug, Functor, etc.
- Generic
- FromJSON, ToJSON
- Maybe an FFI typeclass of some sort?

I want to leave the door open to support deriving of user-defined typeclass
instances via Generics, but there won't initially be any support for that.

### Warnings

Kite will have a small set of sensible warnings, all of which are enabled by
default. These include:
* Incomplete pattern matches
* Unused variables
* Unused imports 
* Duplicate exports 
* Unused, unexported functions

## Language

Kite is very similar to Haskell 98, with a few differences intended to make it
less intimidating to programmers from other languages.

The largest difference is that Kite is strict rather than lazy. I've chosen this
option for a few reasons:
- It is more familiar to programmers from other backgrounds
- It gives more predictable memory usage
- It is simpler to compile
- With the exception of infinite recursive datatypes and custom control flow,
  everything you can express in a lazy language can also be expressed in a
  strict one.

Kite has very few infix operators, preferring named functions instead. `map`
instead of `<$>`, brackets instead of `$`. Infix numeric and comparison
operators will remain because they're familiar to everyone, and we still use `.`
for function composition because it's so useful. A named function can be made
infix by placing it in backticks.

## Syntax

Every Kite file defines a module, which begins `module <name>`. The module name
must begin with a capital letter. This is optionally followed by a list of
exported definitions.

```haskell
module Example (fun1, fun2, SomeType, SomeClass)
```

The contents of a Kite module is a series of definitions, which are either
bindings or type definitions.

### Binding Syntax

Top-level bindings are names for expressions. They must have a type signature.
The following example defines two bindings: `zero` has type `Int` and is bound
to the literal value `0`. `isZero` has type `Int -> Bool` (it's a function) and
is bound to a multi-case expression.

```haskell
zero : Int
zero = 0

isZero : Int -> Bool
isZero = 0 -> True
         _ -> False
```

### Case expressions

Case expressions are syntactically identical to Haskell, Elm or Purescript.

```haskell
case my_list of
  [] -> Nothing
  [x] -> Just x
  _ -> Nothing
```

A case expression consists of the keyword `case`, a _target_ which can be any
expression, the keyword `of`, and one or more branches. Each branch consists of
a pattern and a corresponding expression, separated by `->`.

The case expression evaluates the target and selects the first branch whose pattern
matches it. The result of the case expression is the right hand side of the
selected branch. Branches are checked in order from top to bottom.

```haskell
case Just 5 of
  Nothing -> False
  Just 2 -> False
  Just x -> True

-- evaluates to True
```

Kite supports several different types of pattern:
- Variable patterns: `x`
- Wildcard patterns: `_`
- Tuple patterns: `(x, y)`, `(x, y, z)`
- List patterns: `[1,2,3]`, `[]`
- List constructor patterns: `x::xs`
- General constructor patterns: `Just x`, `Right y`
- String patterns: `"hello"`

Variable patterns like `x` match any value, but also bind the value to the name
(`x`) in the right hand side. Wildcard patterns `_` match any value but don't
bind it to a name.

### [Multi-case expressions](#multi-case)

Multi-case expressions are like case expressions without a target. They define
functions of one or more arguments. This is best illustrated by an example:

```haskell
mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe =
  _ Nothing  -> Nothing
  f (Just x) -> Just (f x)
```

This multi-case expression has two patterns in each branch, corresponding to the
two arguments `(a -> b)` and `Maybe a`. Multi-case expressions are the primary
way that functions are defined, as Kite has no support for binding variables on
the left hand side of function definitions (as in other Haskell-like languages).

"Lambda syntax" can be approximated by a multi-case expression that has just one
branch with only variable patterns, like `x y -> x + y`.

### Let expressions

In any expression (i.e. anywhere to the right of the `=` in an equation) you can
use let expressions, which declare variables that you can use later on. All
variables in Kite are immutable, so once you delcare them you cannot change their
value.

```
doubleAndAddFive : Int -> Int
doubleAndAddFive = x -> let doubled = x + x in doubled + 5
```

A let expression is one or more bindings of the form `variableName =
variableValue`, separated by newlines, followed by a final expression which is
the _body_ of the let. The body is the expression that the let will evaluate to.
Here is an example with multiple bindings:

```
let a = 1
    b = 2
    c = a + b
 in c + c
```

This expression simplifies to `(1 + 2) + (1 + 2)` and hence to `6`.

### Where clauses

Where clauses allow you to define helper functions that are only available in
the scope of some particular top level function. For example:

```haskell
init : [a] -> Maybe [a]
init = [] -> Nothing
init = xs -> Just (helper xs)
 where helper : [a] -> [a]
       helper = [x]   -> []
                x::xs -> x :: helper xs
```

The top level function defined here is `init` and it calls the helper
function `helper` which is defined in a where clause. `helper` is only in scope
inside `init`. Unlike Haskell, `helper` cannot see any variables bound in
`init`, including its arguments.

You can think of `where` as defining a top level function which can only be used
by the parent function.

### Type definitions

Type definitions look like this:

```haskell
type Action
 = DoNothing
 | StoreInt Int
 | Transform (Int -> Int)
 | CompareAndSwap Int Int

type Eq a = Eq (a -> a -> Bool)
```

This should be familiar to users of Haskell, Elm or Purescript, except that the
keyword is `type` rather than `data`.

## Records

Kite has Haskell's ADTs and records, but aims to provide better ergonomics around
record field selection. Out of the box, Haskell doesn't deal well with multiple
record types having the same field name. There are a variety of solutions to
this, each with their own tradeoffs. Kite aims to find one which is both simple
and unsurprising, though the exact solution is not yet settled.

### Solution 1. Dot notation for record selectors
This solution adds a new syntax form for accessing a field: `.field`. Given a
record `foo` with a field `bar`, you can extract the value of the field with
`foo.bar`. How this is supported isn't decided, but it will probably be stolen
from [TDNR](https://wiki.haskell.org/TypeDirectedNameResolution) or the recent
[GHC proposals](https://github.com/ghc-proposals/ghc-proposals/pull/282) on
record fields.

An example:
```haskell
type User = User { name : String, age : Int }

users = [User { name = "Kurt", age = 114}, User { name = "Alan", age = 108}]

userNames = map (.name) users

kurtAge = (head users).age

olderUsers = filter ((> 110) . .age) users
```

We require no spaces between the dot and the field name in `.field` to
disambiguate between this and function composition.

### Solution 2. Record selection without dot notation
This solution introduces no new syntax. Record selectors look just like normal
functions, but behind the scenes they exist in a separate namespace defined by
the record type they belong to. When applied to an argument, the type of the
argument determines the namespace that the compiler will infer for the function,
and therefore what specific selector is chosen. For example:

```haskell
type User = User { name : String, age : Int }
-- internally, this generates
-- User.name : User -> String
-- User.age : User -> Int

kurt = User { name = "Kurt", age = 114 }

-- age is resolved to User.age because kurt : User
kurtAge = age kurt

type Animal = Animal { age : Int, mammal : Bool }

whale = Animal { age = 60, mammal = True }

-- age is resolved to Animal.age because whale : Animal
whaleAge = age whale
```

### Solution 3. First class extensible records
Instead of following Haskell's lead, we can make a cleaner break and replace
Haskell style records with anonymous extensible records similar to Elm. This
is likely to closely follow the approach described in "Lightweight Extensible
Records for Haskell" by Mark P. Jones and Simon Peyton-Jones.

### Other language features

Kite has some syntactic sugar borrowed from Haskell and Ruby:
```haskell
-- Haskell style comments
-- # Markdown support
-- - lists
-- - etc.
--
-- inline `code` and
--
-- ```
-- multiline code blocks
-- between triple backticks
-- ```

-- Tuple literals
tuple = (1, 2, True)

-- List literals, including a special [a] syntax for list types
aList : [Integer]
aList = [1, 2, 3]

-- String interpolation
-- The interpolated variable will be converted to a string via the Show typeclass.
-- Interpolating a variable which doesn't have a Show instance is a type error.
aString = "this is a tuple: #{tuple}"

-- use """ to delimit multiline strings
help = """
  Welcome to the Kite REPL.
  For general help, type :?
  To see documentation on a function or type, use :info
  Type :quit to exit.
  """
```

In the future there may also be some sugar for list ranges, like `[1..100]`.

All string literals will be overloaded by default, meaning their type will be
```
StringLike a => a
```
This will allow libraries to use string literals for their own types, e.g. for
safe SQL queries.

## Testing

Kite supports at least two types of tests: doctests and test functions.

### Doctests

Doctests are comments formatted in a particular way that are parsed and
converted into tests. They're a feature of Rust and Elixir, amongst other
languages.

```haskell
-- ```
-- head []             == error "head: empty list"
-- head [1,2,3]        == 1
-- head ["foo", "bar"] == "foo"
-- ```
head : [a] -> a
head = []    -> error "head: empty list"
head = x::xs -> x
```

Any code blocks in comments in a Kite module will be extracted and run in the scope
of the module itself. Each line in the code block is expected to be of type
Bool, and `kite test` will expect every expression to evaluate to `True`. A test
is considered failing if it evaluates to False or throws an error.

This might need some rethinking for more complex tests, but it should work for
simple stuff.

### Test functions

Test functions are like normal tests, but they have a specific type (probably
`Test` or `Test ()` or something) and the compiler can automatically discover them
and run them - you don't need to set up a test harness.

## Interpretation and Compilation

Kite will be interpreted like Ruby, but also support compilation to a static
binary (via Go) and a Javascript file.  Go and JS compilation will be a bit like
program extraction, as the main function for each will be different and will
exist in a different monad, to express the fact that different effects are
available. As a result you'll be able to write your frontend and backend app in
the same project, sharing lots of code, and just compile each by specifying a
different flag to `kite build`.

Example:
```haskell
-- alert : String -> IO JS ()
-- putLine : String -> IO Haskell ()
-- println : String -> IO Go ()

frontendMain : IO JS ()
frontendMain =
  alert "this is a browser alert message. The 5th fibonacci number is #{fib 5}"
  
backendMain : IO Haskell ()
backendMain =
  putLine "this is a message written to stdout. The 5th fibonacci number is #{fib 5}"

-- In practice this is tedious - Kite will have standard library modules that
-- abstract over the backend.
compiledMain : IO Go ()
compiledMain =
  println "this is a message written to stdout. The 5th fibonacci number is #{fib 5}"

-- any code outside the IO monad can be shared between backends
fib : Int -> Int
fib = 
  0 -> 0
  1 -> 1
  n -> fib (n - 2) + fib (n - 1)
```

JS compilation will produce a single minified JS file that exports your main
function. In addition, you can compile any individual Kite module to JS provided
it doesn't export any IO functions.

# Standard Library

The standard library is batteries included, and similar to Ruby's. It has
most of Haskell's base libraries plus modules for JSON, HTTP, logging, etc.

You should be able to write most scripts with just the standard library, and
only need packages for larger applications.

# Tooling

All Kite tools will be distributed in a single `kite` binary.

Kite will have a code formatter with a single style, a la gofmt.
The formatter will insert type annotations for top level declarations if they
are missing (and inferrable).

As mentioned at the start, Kite files can optionally have a YAML frontmatter
section at the top. The syntax of this is a restricted subset of YAML that just
supports keys and primitive values (this is just to prevent you doing crazy
things with it). It's intended to be a lightweight and flexible store for
metadata about the file, such as the code owner, contact information, license
etc. Kite tooling will be able to extract and process this metadata.

Kite source files have a `.kite` extension.

## Packaging

Kite's packaging system will be similar to Ruby and Bundler but integrated
tightly into the language. You'll be able to create packages locally and push
them up to a Kite package server, which will index them and generate
documentation (and possibly run tests). Versioning is SemVer and uses lockfiles
for consistency. You'll also be able to run the server locally with no extra
setup.

There are a number of additional features that make life easier for maintainers
and users of Kite packages.

### Breaking change detection

Like Elm, Kite will detect when your your package contains a breaking change from
a previous version and prevent you from releasing that change as a patch or
minor version.

### Breakage prediction

For a widely used package it can often be difficult to know the impact of a
breaking change until you release it and receive feedback from users. Kite will
allow you to automatically build every package in the ecosystem that depends on
yours and thereby predict the amount of churn before you release the new
version.

### Global search

The Kite package server will support type-directed (i.e. Hoogle style) search
across the entire package ecosystem. If a package somewhere has a function to
convert a type from package A into a type from package B, you'll be able to find
it.

### Upper bound management

In some parts of the Haskell world, a lot of time is spent setting and updating
upper bounds on package dependencies. Unmaintained (but useful) packages cause
issues if their upper bounds don't get updated as new versions of their
dependencies are released. Kite will attempt to automate the setting and updating
of upper bounds by determining the highest version at which the package builds
successfully. This should remove a lot of work on the part of package
maintainers.

### Safe builds

Kite intends to have no support for running arbitrary code at build time. As a
result, it should always be safe to build any Kite package in any environment.

### Editor tools

Kite will have native Language Server Protocol support, and this will be used to
implement interactive editing features such as case split. Kite will also ship
with a simple `ghcid` style command (`kite watch` or something) that gives near
instant feedback on type errors.

# Performance

Much of the tooling described above depends heavily on typechecking being fast.
One of the reasons this tooling is less common in other languages is the time it
takes to build packages and find type errors. Kite's typechecker will be
optimised to complete as quickly as possible, and Kite is willing to trade off
more advanced features in exchange for performance in this area.

Full build performance and runtime performance is a lower priority. Kite's
interpreter will aim to be as fast as Ruby's. Compiled performance should be
stronger, but is unlikely to compete with Haskell or Go. Kite will make use of
common functional programming language optimisations such as inlining and beta
reduction, but won't have anywhere near GHC's level of advanced optimisation.

# Runtime

Being a garbage collected language, Kite needs a runtime of some description.
The interpreter will piggyback off Haskell's runtime, and offer bindings to
GHC's concurrency primitives (green threads, IORefs, MVars, STM etc). When
compiled to Go, Kite will leverage the Go runtime and can use the concurrency
primitives available there (goroutings, channels etc).

# Exceptions

Note: In the absence of a multithreaded runtime I am not sure whether it's necessary
to have comprehensive exception support. Kite's initial behaviour will be the
following, but this could change in the future.

Kite has no exceptions. There are four ways that a Kite program can halt:
- The program calls `exit : IO ()` or a similar function to exit cleanly.
- The program encounters an `error` call and panics.
- The program receives a signal to halt, such as SIGQUIT. Unless a custom signal
  handler is installed, it panics.
- The program encounters an unrecoverable error such as a memory allocation
  failure, and panics. If running in the interpreter this may be originally
  triggered by an async Haskell exception but Kite will panic just the same.

When a Kite program panics, the following happens:
- The stack is immediately unwound, incrementally printing a stack trace to stderr.
- The program exits with a non-zero exit code.

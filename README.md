# Lam

> Ruby and Haskell have a baby

Lam is a statically typed, strict, purely functional programming language with full type inference.
It's the core parts of Haskell with better ergonomics, an interpreter, native JS
support and much better tooling.

Lam targets the Ruby use case: web applications and scripts. For teams building big
Rails apps that are struggling with stability and productivity as their codebase
grows, Lam should provide a compelling alternative.

Lam is also intended to be a gateway drug to Haskell. Migration from Lam to
Haskell should be feasible. Lam is _not_ intended to replace Haskell. Its type
system is much less powerful than GHC's and its performance will be much lower.
If you're happy with Haskell then please stick with it!

An example Lam program:

```haskell
---
owner: hmac
description: A hello program
---
module Main

main : IO ()
main = do
  name <- readLine
  greet name

greet : String -> IO ()
greet name = putLine "Hello #{name}!"
```

A few things immediately jump out:
- Lam files support arbitrary YAML frontmatter that you can use for metadata
- Lam syntax is very close to Haskell
- There are small differences, such as a single colon for type annotations
- Lam has Ruby-esque string interpolation

## Type system

Lam's type system is Hindley-Milner plus typeclasses. It should be entirely
unexciting for anyone coming from Haskell, but the addition of typeclasses makes
it more powerful than Elm.

### Typed Holes

Lam supports typed holes in both terms and types, like this:
```haskell
foo : ?1 -> String
foo x = ?2 x
```

All holes begin with a question mark. The compiler will attempt to infer the type of `?1` and `?2`. Holes in types must be filled for typechecking to succeed, but holes in terms may be left (and will generate a warning). If the program encounters a term hole at runtime it will throw an error. This is quite useful if you have half a program written and just want to try it out quickly.

### Totality

Lam has a totality checker that will mark functions as total if it can be sure that they are. A function is considered total if it:
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

Lam will also have a concept of module safety and a safety checker. This is
similar to [Safe Haskell](https://wiki.haskell.org/Safe_Haskell). A safe module
is guaranteed not to perform IO or FFI and therefore poses no security risk. The
worst it can do is put your program in loop or throw an error. Therefore you can
depend on a safe package without worrying about it stealing your secrets in
production, and the automated update of safe dependencies poses no security risk
either. A module is safe if none of the functions defined in it use IO or FFI.

### Typeclass deriving

Lam will be able to automatically generate typeclass instances for some common typeclasses:
- Eq, Show, Read, Functor, etc.
- Generic
- FromJSON, ToJSON
- Maybe an FFI typeclass of some sort?

I want to leave the door open to support deriving of user-defined typeclass
instances via Generics, but there won't initially be any support for that.

## Language

Lam is very similar to Haskell 98, with a few differences intended to make it
less intimidating to programmers from other languages.

The largest difference is that Lam is strict rather than lazy. I've chosen this
option for a few reasons:
- It is more familiar to programmers from other backgrounds
- It gives more predictable memory usage
- It is simpler to compile
- With the exception of infinite recursive datatypes and custom control flow,
  everything you can express in a lazy language can also be expressed in a
  strict one.

However I'm not wedded to this choice. If it turns out that Lam would really
benefit from laziness then it might change in the future.

Lam has very few infix operators, preferring named functions instead. `map`
instead of `<$>`, brackets instead of `$`. Infix numeric and comparison
operators will remain because they're familiar to everyone, and we still use `.`
for function composition because it's so useful. A named function can be made
infix by placing it in backticks.

Lam has Haskell's ADTs and records, with the addition of a `.field` syntax for
accessing record fields. I aim to steal a lot from the recent [GHC
proposals](https://github.com/ghc-proposals/ghc-proposals/pull/282) on record
fields.

An example:
```haskell
data User = { name : String, age : Int }

users = [User { name = "Harry", age = 26}, User { name = "Bob", age = 100}]

userNames = map (.name) users

harryAge = (head users).age

oldUsers = filter ((> 80) . .age) users
```

We require no spaces between the dot and the field name in `.field` to
disambiguate between this and function composition.

Lam has some syntactic sugar borrowed from Haskell and Ruby:
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

-- Pattern matching with sugar for tuples and lists
length : [a] -> Integer
length []       = 0
length (x : xs) = 1 + length xs

-- _ for wildcard patterns
snd : (a, b) -> b
snd (_, x) = x
```

In the future there may also be some sugar for list ranges, like `[1..100]`.

All string literals will be overloaded by default, meaning their type will be
```
IsString a => a
```
This will allow libraries to use string literals for their own types, e.g. for
safe SQL queries.

## Testing

Lam supports at least two types of tests: doctests and test functions.

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
head [] = error "head: empty list"
head (x : xs) = x
```

Any code blocks in comments in a Lam module will be extracted and run in the scope
of the module itself. Each line in the code block is expected to be of type
Bool, and `lam test` will expect every expression to evaluate to `True`. A test
is considered failing if it evaluates to False or throws an error.

This might need some rethinking for more complex tests, but it should work for
simple stuff.

### Test functions

Test functions are like normal tests, but they have a specific type (probably
`Test` or `Test ()` or something) and the compiler can automatically discover them
and run them - you don't need to set up a test harness.

## Interpretation and Compilation

Lam will be interpreted like Ruby, but also support compilation to a static C
binary and a JS file.  C and JS compilation will be a bit like program
extraction, as the main function for each will be different and will exist in a
different monad, to express the fact that different effects are available. As a
result you'll be able to write your frontend and backend app in the same
project, sharing lots of code, and just compile each by specifying a different
flag to `lam build`.

Example:
```haskell
-- alert : String -> JS ()
-- putLine : String -> IO ()

frontendMain : JS ()
frontendMain = do
  let n = fib 5
  putLine "this is a message written to stdout. The 5th fibonacci number is #{n}"
  ...
  
backendMain : IO ()
backendMain = do
  let n = fib 5
  alert "this is a browser alert message. The 5th fibonacci number is #{n}"
  ...

-- any code outside the IO or JS monads can be shared between backends
fib : Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)
```

JS compilation will produce a single minified JS file that exports your main
function. In addition, you can compile any individual Lam module to JS provided
it doesn't export any IO functions.

# Standard Library

The standard library is batteries included, and similar to Ruby's. It has
most of Haskell's base libraries plus modules for JSON, HTTP, logging, etc.

You should be able to write most scripts with just the standard library, and
only need packages for larger applications.

# Tooling

All Lam tools will be distributed in a single `lam` binary.

Lam will have a code formatter with a single style, a la gofmt.
The formatter will insert type annotations for top level declarations if they
are missing (and inferrable).

As mentioned at the start, Lam files can optionally have a YAML frontmatter
section at the top. The syntax of this is a restricted subset of YAML that just
supports keys and primitive values (this is just to prevent you doing crazy
things with it). It's intended to be a lightweight and flexible store for
metadata about the file, such as the code owner, contact information, license
etc. Lam tooling will be able to extract and process this metadata.

Lam source files have a `.lam` extension.

## Packaging

Lam's packaging system will be similar to Ruby and Bundler but integrated
tightly into the language. You'll be able to create packages locally and push
them up to a Lam package server, which will index them and generate
documentation (and possibly run tests). Versioning is SemVer and uses lockfiles
for consistency.

The `lam` tool will be able to serve docs locally over HTTP, and will come with
a type directed search tool a la Hoogle.

# Performance

Lam's parsing and typechecking will be as fast as possible to support a
productive feedback loop. `lam` will support file watching and reloading a la
ghcid.

Lam's runtime performance under interpretation will aim to be on par with
Ruby's. Performance isn't a priority, and Lam is willing to sacrifice some to
gain ergonomics or implementation simplicity.

Compiled performance should be stronger, but is unlikely to compete with GHC,
Java or C. Lam will make use of common functional programming language
optimisations such as inlining and beta reduction, but won't have anywhere near
GHC's level of advanced optimisation.

# Runtime

I'm not sure what Lam's runtime will look like. It is garbage collected, so we
need a runtime of some description, but it's not something I have experience
building. Multithreading would be a nice feature to have, especially as purity
makes it a lot easier to use, but we can live without it initially while still
remaining competitive with Ruby.

I would be very interested in ideas for a simple but performant multithreaded
runtime system.

# Mutable references

Haskell has `IORef`s and `MVar`s to safely model mutable state. Lam probably
needs something like this as well to be generally useful, but I haven't given
this much thought.

# Exceptions

Note: In the absence of a multithreaded runtime I am not sure whether it's necessary
to have comprehensive exception support. Lam's initial behaviour will be the
following, but this could change in the future.

Lam has no exceptions. There are four ways that a Lam program can halt:
- The program calls `exit : IO ()` or a similar function to exit cleanly.
- The program encounters an `error` call and panics.
- The program receives a signal to halt, such as SIGQUIT. Unless a custom signal
  handler is installed, it panics.
- The program encounters an unrecoverable error such as a memory allocation
  failure, and panics.

When a Lam program panics, the following happens:
- The stack is immediately unwound, incrementally printing a stack trace to stderr.
- The program exits with a non-zero exit code.

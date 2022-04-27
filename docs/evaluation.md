# Evaluation

If we want to run Kite code without compiling it to some existing language and
relying on that language's runtime, we need to design an appropriate VM for
Kite.

We currently have an interpreter which evaluates a surface Kite AST in Haskell,
but it is slow. I think a better approach would be to compile Kite to some kind
of simpler language which is still high-level enough to not worry about memory
management.

Firstly, what happens if we simplify Kite as much as possible, leaving only the
fundamental features? At the same time, we'll remove as much ambiguous syntax as
we can.

```
e    = a                                       atomic expression
     | \x -> { e1 }                            abstraction
     | let x = e1 in e2                        let
     | case e0 of {p1 -> e1, ...}              case
     | e1.l1                                   record projection
     | #[ l1 = e1, l2 = e2, ... ]              record literal
     | "<string>"                              string literal
     | '<char'                                 char literal
     | <int>                                   int literal
     | <bool>                                  bool literal

a    = x                                       variable
     | C                                       constructor
     | a1 a2                                   application

p    = C x1 x2 ...                             constructor pattern
     | x1                                      variable pattern
```


Kite:
```
last : [a] -> Maybe a
last = [] -> Nothing
       [x] -> Just x
       (x :: xs) -> last xs
```

Simplified Kite:
```
last = \l -> {
  case l of {
    #Nil -> Nothing,
    #Cons x #Nil -> Just x,
    #Cons x xs   -> last xs
  }
}
```

Kite:
```
init : [a] -> Maybe [a]
init = [] -> Nothing
       xs -> Just (go xs)
 where
  go : [a] -> [a]
  go = [x]       -> []
       (x :: xs) -> x :: (go xs)
```

Simplified Kite:
```
init =
  let go = \l -> {
    case l of {
      #Cons x #Nil -> #Nil
      #Cons x xs   -> let r = go xs in #Cons x r
    }
  }
  in
    \l -> {
      case l of {
        #Nil -> Nothing,
        xs   -> let r = go xs in Just r
      }
    }
```

One notable feature of Simplified Kite is that applications can only contain
"atomic" expresions: variables, constructors or other applications. The effect
of this is to establish a clear evaluation order, because we don't have complex
expressions nested inside applications. Instead, these are first bound in
`let`s.

This makes it possible to write an imperative sequence of instructions for a
Simplified Kite expression. For example:

Simplified Kite:
```
case l of {
  #Nil -> Nothing,
  #Cons x #Nil -> Just x,
  #Cons x xs   -> last xs
}
```

Instructions:
```
case l [ #Nil _1, #Cons _2 ]

_1:
ret Nothing

_2:
case l.1 _4 [ #Nil _3 ]

_3:
r <- con Just l.0
ret r

_4:
r <- app last l.1
ret r
```

Here we assume that `case <val> <default loc> [<val> <loc>, ...]` behaves like a
`switch` statement, that we can access the nth (0-indexed) argument to a constructor with
`c.n`, that `app` performs function application and `con` performs constructor
application. `_foo` is an instruction label. `x <- instr arg0 arg1` stores the
result of the instruction call in the variable `x`. `ret e` returns the value
`e` to the caller.

It's difficult to see how to get all the way from Simplified Kite to this
representation, so let's try a middle ground: a simple C-like language.

KiteC:
```
case l {
  #Nil -> {
    return Nothing;
  }
  #Cons -> {
    case l.1 {
      #Nil -> {
        let r = Just l.0;
        return r;
      }
      default -> {
        let r = last xs;
        return r;
      }
    }
  }
}
```

The key difficulties with interpreteting Kite are closures are partial
application, so let's try an example with those.

Kite:
```
map = _ [] -> []
      f (x :: xs) -> (f x) :: (map f xs)

add = x y -> x + y

incList = l -> map (add 1) l
```

Simplified Kite:
```
map = \f -> {
  \l -> {
    case l of {
      #Nil -> #Nil,
      #Cons x xs -> let x1 = f x 
                     in let xs1 = map f xs
                         in #Cons x1 xs1
    }
  }
}

add = \x -> {
  \y -> {
    x + y
  }
}

incList = \l -> { let f = add 1 in map f l }
```

KiteC:
```
fn map(f, l) {
  case l {
    #Nil -> {
      return #Nil;
    }
    #Cons -> {
      let x1 = f l.0;
      let xs1 = map f l.1;
      let r = #Cons x1 xs1;
      return r;
    }
  }
}

fn add(x, y) {
  let r = x + y;
  return r;
}

fn incList(l) {
  let f = add 1;
  let r = map f l;
  return r;
}
```

So the question is, what do we do when evaluating `add 1`? First, we must assume
that `add` is in normal form, and is a function. We check the arity of `add`,
which must be stored in `add` somewhere, and see that it is 2. We know we can't
call `add` yet, as we have only one argument, so we construct a `PartialApp`
containing `add` and the arguments to it that we have collected so far (`1`).
This `PartialApp` is what is stored in `f`.

When `map f l` is evaluated, we do the same thing. We see that `map` has arity 2
and we have 2 arguments, so we can directly call `map`. We push `l` and `f` on
to the stack and jump to `map`.

`map` may then evaluate `f l.0`. We inspect `f` and see it is a `PartialApp`,
and that (with the addition of `l.0`) it now has the correct number of
arguments. So we push `l.0` and `1` on to the stack and jump to `add`, etc.

How about returning a closure? For example:

Kite:
```
f = x l -> map (y -> y + x) l
```

Simplified Kite:
```
f = \x -> {
  \l -> {
    let g = \y -> { y + x }
     in map g l
  }
}
```

KiteC:
```
fn f(x, l) {
  let g = f_1 x;
  let r = map g l;
  return r;
}

fn f_1(x, y) {
  let r = y + x;
  return r;
}
```

In KiteC we have lambda-lifted `(y -> y + x)` to its own top-level function,
taking an extra argument for the captured variable `x`.

Let's say we evaluate `f 1 l` for some non-empty `l`. We evaluate `f_1 1` which
gives `PartialApp(f_1, [1])`. We then evaluate `map g l`, which eventually
evaluates `PartialApp(f_1, [1]) l.0`. We check the arity of `f_1` (2), see that
we now have 2 arguments, and call `f_1` directly.

Another example:

Kite:
```
fs = x -> [y -> y + x, y -> y - x]

g = map (f -> f 2) (fs 1)
```

Simplified Kite:
```
fs = \x -> {
  let e1 = \y -> { y + x }
   in let e2 = \y -> { y - x }
       in let l1 = #Cons e2 #Nil
           in #Cons e1 l1
}

g = let h = \f -> { f 2 }
     in let xs = fs 1
         in map h xs
```

KiteC:
```
fn fs(x) {
  let e1 = fs_1 x;
  let e2 = fs_2 x;
  let l1 = #Cons e2 #Nil;
  let r = #Cons e1 l2;
  return r;
}

fn fs_1(x, y) {
  let r = y + x;
  return r;
}

fn fs_2(x, y) {
  let r = y - x;
  return r;
}

fn g() {
  let h = g_1;
  let xs = fs 1;
  let r = map h xs;
  return r;
}

fn g_1(f) {
  let r = f 2;
  return r;
}
```

This seems to work fine, provided we continue to ignore the problem of memory
management: i.e. when to allocate and free memory.

## Memory management

We have two options: garbage collection and reference counting. Writing a
garbage collector is a massive undertaking, so I'm inclined to go with reference
counting. We then have two main issues:

- Updating reference counts frequently can take up a lot of CPU time
- We must disallow reference cycles

For the second point, I think we can rule out reference cycles with the type
system. One example of a reference cycle is a circular list. Let's construct
one:

```
let xs = Cons 1 xs
in xs
```

With strict evaluation this expression won't terminate, so we don't really have
to worry about reference cycles from this.

Koka and Lean both get around this problem by (mostly) not allowing cyclic data
structures. Koka has both inductive and co-inductive data structures, so all
lists are finite and there is a separate co-inductive type for infinite streams.

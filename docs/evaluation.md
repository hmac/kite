# Evaluation

If we want to run Kite code without compiling it to some existing language and
relying on that language's runtime, we need to design an appropriate VM for
Kite.

We currently have an interpreter which evaluates a surface Kite AST in Haskell,
but it is slow. I think a better approach would be to compile Kite to some kind
of simpler language which can be evaluated without much overhead.

Firstly, let's simplify Kite as much as possible, leaving only the fundamental
features. We'll convert all multi-case expressions to a combination of lambdas
and normal cases. We'll then lambda-lift so all functions are at the top level.
We will restrict case expressions to having a variable as scrutinee, exactly one
branch per constructor, and no nested patterns. The branch order must match the
order of constructors in the type definition. We will also ensure all
applications of constructors are fully saturated, eta-expanding where necessary.
We will ignore records entirely, as I don't know of an efficient compilation
scheme for them in the general case. We can introduce them later, possibly as a
syntactic sugar over existing Kite features.

We'll call this language KiteCore.

```
p    = fn, ...                                 program
fn   = fn <name>(x, ...) { e }                 function
e    = a                                       atomic expression
     | let x = e1 in e2                        let
     | case x {p1 -> e1, ...}                  case
     | "<string>"                              string literal
     | '<char'                                 char literal
     | <int>                                   int literal
     | <bool>                                  bool literal

a    = x                                       variable
     | C a ...                                 constructor application
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

KiteCore:
```
fn last(l) {
  case l {
    #Nil -> Nothing,
    #Cons x xs -> case xs {
                    #Cons _ _ -> last xs
                    #Nil -> Just x
    }
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

KiteCore:
```
fn init(l)  {
  case l {
    #Nil -> Nothing,
    xs   -> let r = init_go xs in Just r
  }
}

fn init_go(l) {
  case l {
    #Cons x #Nil -> #Nil
    #Cons x xs   -> let r = init_go xs in #Cons x r
  }
}
```

One notable feature of KiteCore is that applications can only contain
"atomic" expresions: variables, constructors or other applications. The effect
of this is to establish a clear evaluation order, because we don't have complex
expressions nested inside applications. Instead, these are first bound in
`let`s.

KiteCore gets rid of the extraneous details of Kite, and we can now consider how
to evaluate it. Our virtual machine will have a heap, a stack and a few
specialised registers. We will have the following basic principles:
- All data is heap allocated
- Function arguments and let bindings are stored on the stack
- Function bodies are instruction sequences
- Constructors are n-ary tuples; the first element is the tag
- We use apply-eval for function application (c.f. push-enter)

We'll call this machine (and its language) KiteVM.

```
p     = (l: | instr) ...             program
l     = <name>                       label
instr = ret s                        return
      | push s                       push address onto stack
      | push ctor(i, j) (s ...)      allocate constructor and push onto stack
      | case s (l ...)               case-analyse value at address s
      | call i s                     call function at address s with i arguments
      | prim <op>                    execute a primitive op (args on stack)
s     = stack[i]                     ith stack element
      | s.i                          ith field of constructor at address s
      | l                            label
i, j  = <natural number>
```

A key aspect of KiteVM is the calling convention. Consider the expression
`f x y`. First, note that application arguments must be atomic expressions,
which means that have already been evaluated to normal form. We start by pushing
`y` and `x` onto the stack (in that order). We then `call 2 f`, which tells the
VM that we want to call the function at `f` and we've provided 2 arguments.

`f` can be two different things:
- The address of a static function in the program
- A partial application of some function to some arguments

If `f` is just a static function, we check its arity and then jump to it. When
`f` returns, we pop enough arguments off the stack to match the arity. This
ensures that if we provided more arguments than `f` takes, they are still on the
stack. In this case `f` will have returned a function that we then must apply to
the remaining arguments. So we repeat the process until we have no arguments
remaining. We then return the result.

If we did not provide enough arguments to saturate `f`, we construct a partial
application `Pap(f, m, args)` where `m` is the arity of `f`. We then return
this.

If `f` is a partial application already, we check to see if our arguments
saturate it. If so, we perform the same process as above to evaluate it. If not,
we push our arguments into the `Pap` and return.

In pseudocode:

```
call n f:

1. Save the current instruction location
2. Check to see if `f` is a function or a partial application
3. If it's a function with arity `m`:
  - If `m > n`:
    - Allocate a new partial application `Pap(f, m, stack[0..n-1])`
    - Return
  - If `m = n`:
    - Allocate a new stack frame and jump to `f`
    - When it returns, pop the stack frame and pop `m` arguments off the stack.
    - Return
  - If `m < n`:
    - Allocate a new stack frame and jump to `f`
    - When it returns, pop the stack frame and pop `m` arguments off the stack.
    - Set `f = retval` and goto 2
4. If it's a partial application `Pap(g, m, args)`:
  - If `n + len(args) > m`:
    - Push each arg in `args` onto the stack
    - Allocate a new stack frame and jump to `g`
    - When it returns, pop the stack frame and pop `m` arguments off the stack.
    - set `f = retval` and goto 2
  - If `n + len(args) < m`:
    - Let `args' = args ++ stack[0..n-1]`
    - Set `retval = Pap(g, m, args')`
    - Return
  - If `n + len(args) = m`:
    - Push each arg in `args` onto the stack
    - Allocate a new stack frame and jump to `g`
    - When it returns, pop the stack frame and pop `m` arguments off the stack.
    - Return
```

TODO: consider having `call n s arg ...` and pushing args onto stack as part of
builtin `call` behaviour.
TODO: consider dropping `retval`, and instead leaving return value on top of
stack

Stack frames are just sections of the stack that are owned by a particular
function. When we enter a function we set up a new stack frame, which just means
pushing the current stack pointer onto the stack and saving that stack address
to the stack pointer. When we return from a function, the runtime code for
`call` will pop the stack frame by popping all elements up to the stack pointer,
and then restore the stack pointer, popping that stack element in turn.

This process is a bit complex, but it allows the instruction language to be
simple. Function application is extremely common in Kite so I think it's better
to handle this in one place than have lots of instructions everywhere to do it.

We've mentioned `Pap` nodes: what other data do we deal with? All values in
KiteVM are either addresses, pap nodes or constructors.

```
v = s
  | Pap(s, i, v ...)
  | ctor(i, j, v ...)
```

In `ctor(i, j, v ...)`, `i` is the tag of the constructor and `j` is the number
of fields. Since constructor applications in KiteCore are fully saturated, `i =
len(v ...)` always.

For example, consider`last`:

KiteCore:
```
fn last(l) {
  case l {
    #Nil -> Nothing,
    #Cons x xs -> case xs {
                    #Nil -> Just x
                    #Cons _ _ -> last xs
    }
  }
}
```

KiteVM:
```
last:
case stack[0] _1 _2

_1:
push ctor(0, 0)
ret stack[0]

_2:
case stack[0].2 _3 _4

_3:
push ctor(1, 2) stack[0].1
ret stack[0]

_4:
push stack[0].2
call 1 last
```

Here's an example with `let`s and calls to unknown functions:

KiteCore:
```
fn apply(f, x, y) { f x y }
fn id(_) { id2 }
fn id2(x) { x }
fn main(y) { let x = Just y in apply id x x }
```

KiteVM:
```
apply:
push stack[2]
push stack[2]
call 2 stack[2]
ret

id:
ret id2

id2:
ret stack[0]

main:
push ctor(1, 1) stack[0]
push stack[0]
push stack[1]
push id
call 3 apply
```

Another example with closures and partial application:

KiteCore:
```
fn map(f, l) {
  case l {
    #Nil -> #Nil,
    #Cons x xs -> let x1 = f x 
                   in let xs1 = map f xs
                       in #Cons x1 xs1
  }
}

fn add(x, y) { x + y }

fn incList(l) { let f = add 1 in map f l }
```

KiteVM:
```
map:
case stack[1] map_1 map_2

map_1:
push ctor(0, 0)
ret stack[0]

map_2:
push stack[1].0
call 1 stack[1]
push retval
push stack[2].1
push stack[2]
call 2 map
push retval
push ctor(1, 2) stack[1] stack[0]
ret stack[0]

add:
push stack[1]
push stack[1]                           ; this could be a SWAP instruction to swap top 2 elements of the stack
prim +
ret

incList:
push 1                                  ; how are integers represented?
call add
push retval
push stack[1]
push stack[1]                           ; these pushes can be optimised away
call map
ret
```

How about returning a closure? For example:

KiteCore:
```
fn f(x, l) { let g = f_1(x) in map g l }

fn f_1(x, y) { y + x }
```

KiteVM:
```
f:
push stack[0]
call 1 f_1
push retval
push stack[2]
push stack[1]
call 2 map
ret

f_1:
push stack[1]
push stack[1]
prim +
ret
```

Another example:

Kite:
```
fs = x -> [y -> y + x, y -> y - x]

g = map (f -> f 2) (fs 1)
```

KiteCore:
```
fn fs(x) {
  let e1 = fs_1 x
   in let e2 = fs_2 x
       in let nil = #Nil
           in let l1 = #Cons e2 nil
               in #Cons e1 l1
}

fn fs_1(x, y) { y + x }

fn fs_2(x, y) { y - x }

fn g() {
  let h = g_1
   in let xs = fs 1
       in map h xs
}

fn g_1(f) { f 2 }
```

KiteVM:
```
fs:
push stack[0]
call 1 fs_1
push retval
push fs_2
push ctor(0, 0)
push ctor(1, 2) stack[1] stack[0]
push ctor(1, 2) stack[3] stack[0]
ret stack[0]

fs_1:
push stack[1]
push stack[1]
prim +
ret

fs_2:
push stack[1]
push stack[1]
prim -
ret

g:
push g_1
push 1
call 1 fs
push retval
push stack[0]
push stack[2]
call 2 map
ret

g_1:
push 2
call 1 stack[1]
ret
```

Another:

Kite:
```
c = _ -> (x -> x)

f = c 1 2
```

KiteCore:
```
fn c(_) { c_1 }
fn c_1(x) { x }
fn f() { c 1 2 }
```

KiteVM:
```
c:
call c_1
ret

c_1:
ret stack[0]

f:
push 2
push 1
call c
ret
```

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

Koka's approach to reference counting is Perceus[0], which is able to free
objects as soon as they are no longer required, and can reuse heap objects
when one is created right after destroying another. The end result is a lot of
nice functional programs end up running as fast mutating programs.

The basic philosophy of Perceus is that functions always take ownership of their
arguments. A variable is therefore "used up" by being passed as a function
argument. If you want to use a variable more than once, you must increment its
reference count in order to retain ownership of it. When a variable is no longer
needed you must decrement its reference count.

We will use `inc x` and `dec x` for incrementing and decrementing the
reference count of the variable `x`.

For example, consider this KiteCore:

```
fn last(l) {
  case l {
    #Nil -> Nothing,
    #Cons x xs -> case xs {
                    #Nil -> Just x
                    #Cons _ _ -> last xs
    }
  }
}
```

`l` is case-analysed but not passed to any other function, so we should `dec l`
in each branch of the outer case.
`xs` is brought into scope in the outer `#Cons` branch. We don't own this
reference (c.f. we _do_ own `l`). Therefore we must `inc xs` in this branch.
The same applies to `x`.
In the inner `#Nil` branch, `x` is used by the application of `Just`, but `xs`
remains unused so we must `dec xs`.
In the inner `#Cons` branch, `xs` is used by the application of `last` but `x`
is unused so we must `dec x`.

```
fn last(l) {
  case l {
    #Nil -> { dec l; Nothing },
    #Cons x xs -> {
      inc x;
      inc xs;
      dec l;
      case xs {
        #Nil -> { dec xs; Just x }
        #Cons _ _ -> { dec x; last xs }
      }
    }
  }
}
```

Perceus then applies _dec specialisation_, which inlines a `dec` call on a
specific constructor. `dec` is defined in pseudocode as

```
fn dec(x) {
  if is_unique(x) {
    dec children of x; free x;
  } else {
    decref(x);
  }
}
```

Where `is_unique(x)` is true if the reference count of `x` is 1, `free(x)` frees
the memory at `x`, and `decref(x)` actually decrements the reference count of
`x`.

Perceus specialises a `dec(x)` call if the children of `x` are used. In our
example that applies to the `dec l` call in the outer `#Cons` branch:

```
fn last(l) {
  case l {
    #Nil -> { dec l; Nothing },
    #Cons x xs -> {
      inc x;
      inc xs;
      if is_unique(l) {
        dec x; dec xs; free l;
      } else {
        decref l;
      }
      case xs {
        #Nil -> { dec xs; Just x }
        #Cons _ _ -> { dec x; last xs }
      }
    }
  }
}
```

We can then push the `inc` instructions into the branches of the `if`, yielding:

```
fn last(l) {
  case l {
    #Nil -> { dec l; Nothing },
    #Cons x xs -> {
      if is_unique(l) {
        inc x; inc xs; dec x; dec xs; free l;
      } else {
        inc x; inc xs; decref l;
      }
      case xs {
        #Nil -> { dec xs; Just x }
        #Cons _ _ -> { dec x; last xs }
      }
    }
  }
}
```

And we can then fuse adjacent `inc(x)/dec(x)` calls together, as they cancel
each other out:

```
fn last(l) {
  case l {
    #Nil -> { dec l; Nothing },
    #Cons x xs -> {
      if is_unique(l) {
        free l;
      } else {
        inc x; inc xs; decref l;
      }
      case xs {
        #Nil -> { dec xs; Just x }
        #Cons _ _ -> { dec x; last xs }
      }
    }
  }
}
```

Now in the case that `l` is not shared with any other function (i.e. it has
refcount 1), and assuming we're operating on a `Cons` (more common than `Nil`),
we perform just two memory management operations: `free l` and `dec x`.

Perceus also performs _reuse analysis_, which is best demonstrated with the
`map` function:

```
fn map(f, l) {
  case l {
    #Nil -> #Nil,
    #Cons x xs -> let x2 = f x
                   in let xs2 = map f xs
                       in #Cons x2 xs2
  }
}
```

We look at any `case` expressions, and try to the pair constructor pattern in
each branch with a constructor introduction with the same number of fields. In
this case we can pair the `#Cons x xs` pattern with the `#Cons x2 xs2`
application. If the variable analysed by the case is no longer used in this
branch, we can reuse it. We do this by generating a _reuse token_ by calling
`dec_reuse` instead of calling `dec(l)`. We then pass this token to `#Cons` in
the application.

```
fn map(f, l) {
  case l {
    #Nil -> { dec l; dec f; #Nil },
    #Cons x xs -> {
      inc x;
      inc xs;
      let t = dec_reuse l;
       in let x2 = inc f; f x
           in let xs2 = map f xs
               in #Cons@t x2 xs2
    }
  }
}
```

`#Cons@t x2 xs2` is evaluated by checking if `t` is `NULL`. If so, a new `#Cons`
object is allocated and things proceed as usual. If `t` is not `NULL`, the
object it points to is reused by effectively doing `t.0 = x2; t.1 = xs2`.

The effect of this is that we don't free `l` and we don't need to allocate a new
`#Cons`.

Like `dec`, `dec_reuse` can also be specialised. Its pseudocode definition is

```
fn dec_reuse(x) {
  if is_unique(x) {
    dec children of x; &x;
  } else {
    decref(x); NULL;
  }
}
```

`dec_reuse(x)` returns either the address of `x`, if `x` can be freed, or
`NULL` if it can't. This return value is the _reuse token_ that is checked in
the `#Cons` call.

We can inline this definition and do some more optimisation:

```
fn map(f, l) {
  case l {
    #Nil -> { dec l; dec f; #Nil },
    #Cons x xs -> {
      inc x;
      inc xs;
      let t = if is_unique(l) {
        dec x;
        dec xs;
        &l;
      } else { decref(l); NULL; };
       in let x2 = inc f; f x
           in let xs2 = map f xs
               in #Cons@t x2 xs2
    }
  }
}
```

Push `inc x; inc xs` inside the `if`, and fuse:

```
fn map(f, l) {
  case l {
    #Nil -> { dec l; dec f; #Nil },
    #Cons x xs -> {
      let t = if is_unique(l) {
        &l;
      } else {
        inc x;
        inc xs;
        decref(l);
        NULL;
      };
       in let x2 = inc f; f x
           in let xs2 = map f xs
               in #Cons@t x2 xs2
    }
  }
}
```

Now in the case that `l` is uniquely owned by `map`, for the `#Cons` case we
perform only one memory management operation: `inc f`. We perform no memory
allocation whatsoever, instead we update `l` in place.

## References

[0]: Perceus - garbage free reference counting with reuse (https://xnning.github.io/papers/perceus.pdf)

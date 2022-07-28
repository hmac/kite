# Implicits

A major feature of Kite is implicit parameters, which are special function
parameters which are automatically inserted by the compiler.

The type of a function that takes an implicit value of type A and returns a
value of type B is written

    A => B

Implicit parameters do not have to be bound at the term level, for example

    f : Int => Bool
    f = True

They can be bound explicitly like this

    f : Int => Bool
    f = n => n == 0

The main use of this feature is to emulate type classes, for example:

    -- Type class definition
    type Eq a = Eq (a -> a -> Bool)

    -- Type class method
    eq : Eq a => a -> a -> Bool
    eq = (Eq f) => x y -> f x y

    -- Type class instance
    eqBool : Eq Bool
    eqBool = Eq (True  True  -> True
                 False False -> True
                 _     _     -> False)

    example : Bool
    example = eq True True

To ensure coherence, an implicit parameter can only be inferred when there is a
single unique value of that type in scope. Therefore, for example, defining two
values of type Eq Bool will not compile.

Values to fill an implicit can be any value in scope at the call site. This
includes top-level definitions visible from the module and also arguments to the
function itself, e.g. in

    allEqual : Eq a => [a] -> Bool
    allEqual = []     -> True
               [x]    -> True
               x:y:xs -> and (eq x y) (allEqual xs)

The call to `eq` must be solved using the implict argument of type `Eq a` to `allEqual`.

The compiler expands functions like this to the following:

    allEqual : Eq a -> [a] -> Bool
    allEqual = eqA -> ([]     -> True
                       [x]    -> True
                       x:y:xs -> and (eq eqA x y) (allEqual eqA  xs))

The implicit parameter `Eq a` is bound as an explicit parameter, and this value
is passed to the call sites which expect it.

# Typechecking

Insertion of implicit arguments happens at application nodes. When typechecking
an application

    (f : A => B) x

we insert an implicit node, yielding

    (f : A => B) {{A}} x

The implicit node stores the type it is expected to have, but we don’t try to
search for a matching value yet, as the type may contain unsolved unification
variables.

When typechecking a binding of an implicit parameter

    (x : A) => e

we treat it much like a normal variable binding, checking e with x:A in scope.

When typechecking the application of an implicit binding to a value

    ((x : A) => e) a

we insert an implicit node, and assume that the value is a subsequent argument.

    ((x : A) => e) {{A}} a

So means you can’t actually provide an implicit argument manually - it is always
inferred from the context.

This removes any ambiguity about whether we should insert an implicit argument
in an application or not - the rule is that we do it in every case that we see
an implicit function type.

The downside is that implicits are very constrained, and we can’t really pass
them around like normal values. I’m not sure how this will play out in practice,
but for the type classes use case it should be fine.

# Non-function implicits

Consider this type class

    type Monoid a = Monoid { empty : a }

    empty : Monoid a => a
    empty = (Monoid m) => m.empty

    example : []
    example = empty

The call to `empty` in the example isn’t an application, so we wouldn’t look to
insert an implicit argument. We would check `empty : []` and then try to unify
`Monoid a => a` with `[]`.

It seems we should check for implicit arguments during the infer stage, and
insert them there, before we reach inferApp.

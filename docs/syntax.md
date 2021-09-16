# Kite syntax

Kite's (new) syntax is designed with two goals in mind:

- To be unambiguous and straightforward to parse
- To be familiar to mainstream programmers

This means we don't use whitespace for layout: instead we delimit code blocks
using braces.

## Examples
```kite
module Data.Maybe

from kite import Kite.Prim {Bool, True, False}
import Data.Functor {Functor {*} }
import Data.Eq {Eq, eq}
import Control.Applicative {Applicative, pure, liftA2}
import Control.Monad.State {State, monadState, applicativeState}
import Data.Monad {Monad, bind}
import Data.Function {flip}
import Control.Alternative {Alternative}
import Data.Show {Show {*}, show}

type Maybe a { Just a, Nothing }

eqMaybe : Eq a -> Eq (Maybe a) {
  match {
    eqa -> Eq [
      eq : match {
            Nothing, Nothing -> True,
            Just x,  Just y  -> eq eqa x y,
            _,       _       -> False
          }
    ]
  }
}

showMaybe : Show a -> Show (Maybe a) {
  match {
    showa -> Show [
      show: match {
        mx -> maybe "Nothing" match {x -> "Just #{show showa x}"} mx
      }
    ]
  }
}

mapMaybe : (a -> b) -> Maybe a -> Maybe b {
  match {
    _, Nothing -> Nothing,
    f, Just x  -> Just (f x),
  }
}

functorMaybe : Functor Maybe {
  Functor [map: mapMaybe]
}

apMaybe : Maybe (a -> b) -> Maybe a -> Maybe b {
  match {
    Nothing, _ -> Nothing,
    Just f, m  -> mapMaybe f m
  }
}

applicativeMaybe : Applicative Maybe {
  Applicative [functor: functorMaybe, pure: Just, ap: apMaybe]
}

bindMaybe : Maybe a -> (a -> Maybe b) -> Maybe b {
  match {
    Nothing, _ -> Nothing,
    Just x, f -> f x,
  }
}

monadMaybe : Monad Maybe {
  Monad [applicative: applicativeMaybe, bind: bindMaybe]
}

plusMaybe : Maybe a -> Maybe a -> Maybe a {
  match {
    Nothing, r -> r,
    l,       _ -> l,
  }
}

alternativeMaybe : Alternative Maybe {
  Alternative [applicative: applicativeMaybe, zero: Nothing, plus: plusMaybe]
}

maybe : b -> (a -> b) -> Maybe a -> b {
  match {
    n, _, Nothing -> n,
    _, f, Just x  -> f x
  }
}

isJust : Maybe a -> Bool {
  match {
    Nothing -> False,
    _ -> True,
  }
}

catMaybes : [Maybe a] -> [a] {
  match {
    [] -> [],
    Just x :: rest -> x :: catMaybes rest,
    Nothing :: rest -> catMaybes rest,
  }
}

filterMaybe : (a -> Maybe b) -> [a] -> [b] {
  match {
    _, [] -> [],
    f, x::xs -> match f x {
      Just r -> r :: filterMaybe f xs,
      Nothing -> filterMaybe f xs,
    }
  }
}
```

## Grammar

```ebnf
(* Identifiers *)
qual_upper_ident = upper_ident, {".", upper_ident} ;
qual_lower_ident = {upper_ident, "."}, lower_ident ;
upper_ident = /[A-Z_][A-z0-9_]*/ ;
lower_ident = /[a-z_][A-z0-9_]*/ ;
pkg_ident = /[a-z][a-z0-9]*/ ;

(* Modules and imports *)
module = "module", qual_upper_ident, {import}, {def} ;
import = ["from", pkg_ident], "import", ["open"], qual_upper_ident, [import_list], ["as", qual_upper_ident] ;
import_list = "{", [ import_list_item, {",", import_list_item}, [","] ] "}" ;
import_list_item = (
                    upper_ident,
                    [
                      "{",
                      [ "*" | upper_ident, {",", upper_ident}, [","] ],
                      "}"
                    ]
                  )
                 | lower_ident ;

(* Definitions *)
def = val_def | type_def ;
val_def = lower_ident, ":", type, "{", expr, "}" ;
type_def = "type", upper_ident, [type_params], "{", [ ctor_def, {",", ctor_def}, [","] ], "}" ;
ctor_def = upper_ident, {atomic_type} ;
type_params = lower_ident, {lower_ident} ;

(* Types *)
type = atomic_type | function_type | application_type ;
atomic_type = record_type | list_type | ctor_type | var_type | "(", type, ")" ;

function_type = type, "->", type ;
application_type = atomic_type, atomic_type, {atomic_type} ;
record_type = "[", ( ":" | record_type_pair, {",", record_type_pair}, [","] ), "]" ;
record_type_pair = lower_ident, ":", type ;
list_type = "[", type, "]" ;
ctor_type = qual_upper_ident ;
var_type = lower_ident ;

(* Expressions *)
expr = atomic_expr | infix_expr | application_expr ;
atomic_expr = match | ctor | let | var | list | record | record_projection
            | tuple | int | string | "(", expr, ")" ;

application_expr = atomic_expr, atomic_expr, {atomic_expr} ;
int = /[0-9]+/ ;
ctor = qual_upper_ident ;
var = qual_lower_ident ;
list = "[", [expr, {",", expr}, [","]], "]" ;
tuple = "(", ",", [expr, {",", expr}, [","]], ")" ;
record = "[", (":" | [record_pair, {",", record_pair}, [","]]), "]" ;
record_pair = lower_ident, ":", expr ;
record_projection = atomic_expr, <no whitespace>, lower_ident ;
let = "let", [let_pair, {",", let_pair}, [","]], "{", expr, "}" ;
let_pair = lower_ident, "=", expr ;

(* Match expressions *)
match = "match", [expr], "{", [match_branch, {",", match_branch}, [","]], "}" ;
match_branch = pattern_group, "->", expr ;
pattern_group = pattern, {",", pattern} ;

(* Patterns *)
pattern = ctor_pattern | wildcard_pattern | list_pattern | cons_pattern | var_pattern ;
ctor_pattern = ctor, {pattern} ;
wildcard_pattern = "_" ;
list_pattern = "[", {pattern}, "]" ;
cons_pattern = pattern, "::", pattern ;
var_pattern = lower_ident ;

(* Infix expressions *)
infix_expr = expr, infix_op, expr ;
infix_op = "+" | "-" | "*" | "/" | "::" ;

(* Strings *)
string = <ruby style strings with #{...} interpolation> ;
```

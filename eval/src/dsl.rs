use std::rc::Rc;

use crate::eval::*;

/// A DSL for constructing `NamedExpr` values, useful for testing

pub fn local(name: &str) -> NamedVar {
    NamedVar::Local(name.into())
}

pub fn arg(name: &str) -> NamedVar {
    NamedVar::Arg(name.into())
}

pub fn global(name: &str) -> NamedVar {
    NamedVar::Global(name.into())
}

pub fn let_(name: &str, value: NamedExpr, body: NamedExpr) -> NamedExpr {
    NamedExpr::Let(name.into(), Box::new(value), Box::new(body))
}

pub fn var(v: NamedVar) -> NamedExpr {
    NamedExpr::Var(v)
}

pub fn ctor(name: &str, tag: usize) -> NamedCtor {
    NamedCtor {
        name: name.into(),
        tag,
    }
}

pub fn ctor_(name: &str, tag: usize, args: Vec<NamedVar>) -> NamedExpr {
    NamedExpr::Ctor(
        NamedCtor {
            name: name.into(),
            tag,
        },
        args,
    )
}

pub fn case(target: NamedVar, alts: Vec<(Pat, NamedExpr)>) -> NamedExpr {
    NamedExpr::Case(target, alts)
}

pub fn app(f: NamedVar, args: Vec<NamedVar>) -> NamedExpr {
    NamedExpr::App(f, args)
}

pub fn ctor_pat(name: &str, tag: usize, args: Vec<&str>) -> Pat {
    Pat::Ctor(
        ctor(name, tag),
        args.into_iter().map(String::from).collect(),
    )
}

pub fn prim(p: Prim, args: Vec<NamedVar>) -> NamedExpr {
    NamedExpr::Prim(p, args)
}

pub fn int(i: i32) -> NamedExpr {
    NamedExpr::Int(i)
}

pub fn inc(v: NamedVar, expr: NamedExpr) -> NamedExpr {
    NamedExpr::Inc(v, Box::new(expr))
}

pub fn dec(v: NamedVar, expr: NamedExpr) -> NamedExpr {
    NamedExpr::Dec(v, Box::new(expr))
}

pub fn def(name: &str, params: Vec<&str>, expr: NamedExpr) -> Def<NamedExpr> {
    let arity = params.len();
    Def {
        name: name.into(),
        params: params.into_iter().map(String::from).collect(),
        arity,
        expr,
    }
}

pub fn val_ctor<'a>(tag: usize, args: Vec<Val<'a>>) -> Val<'a> {
    Val::Ctor(
        Ctor { tag },
        args.into_iter().map(RcVal::new).map(Rc::new).collect(),
    )
}

pub fn val_int<'a>(n: i32) -> Val<'a> {
    Val::Int(n)
}

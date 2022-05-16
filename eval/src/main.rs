use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

fn main() {
    println!("hello!");
}

#[derive(Clone, Debug)]
enum NamedExpr {
    Var(NamedVar),
    App(NamedVar, Vec<NamedVar>),
    Let(String, Box<NamedExpr>, Box<NamedExpr>),
    Case(NamedVar, Vec<(Pat, NamedExpr)>),
    Ctor(NamedCtor, Vec<NamedVar>),
    Int(i32),
    Prim(Prim, Vec<NamedVar>),
}

#[derive(Clone, Debug)]
enum Pat {
    Ctor(NamedCtor, Vec<String>),
}

#[derive(Clone, Debug)]
enum NamedVar {
    Global(String),
    Local(String),
    Arg(String),
}

#[derive(Clone, Debug)]
struct NamedCtor {
    name: String,
    tag: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Prim {
    IntAdd,
    IntSub,
    IntEq,
    IntLt,
}

// A nameless expression.
// Variables are indices.
#[derive(Debug, PartialEq, Eq)]
enum Expr {
    Var(Var),
    App(Var, Vec<Var>),
    Let(Box<Expr>, Box<Expr>),
    Case(Var, Vec<(Ctor, Expr)>),
    Ctor(Ctor, Vec<Var>),
    Int(i32),
    Prim(Prim, Vec<Var>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Var {
    // TODO: this is slow; store reference to Def instead
    Global(String),
    Local(usize),
    Arg(usize),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Ctor {
    tag: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Def<E> {
    name: String,
    arity: usize,
    params: Vec<String>,
    expr: E,
}

struct Env<E>(Vec<Def<E>>);

struct LazyEnvConverter {
    named: RefCell<HashMap<String, Rc<Def<NamedExpr>>>>,
    nameless: RefCell<HashMap<String, Def<Expr>>>,
}

impl LazyEnvConverter {
    fn new(env: Vec<Def<NamedExpr>>) -> Self {
        let mut named = HashMap::new();
        for def in env {
            named.insert(def.name.clone(), Rc::new(def));
        }
        Self {
            named: RefCell::new(named),
            nameless: RefCell::new(HashMap::new()),
        }
    }
    fn convert(&self, name: &str) {
        if self.nameless.borrow().contains_key(name) {
            return;
        }
        match self.named.borrow().get(name) {
            None => panic!("No def with name {}", name),
            Some(def) => {
                let nameless_def = make_nameless_def(self, &def);
                self.nameless.borrow_mut().insert(name.into(), nameless_def);
            }
        };
    }

    fn into_nameless_env(self) -> HashMap<String, Def<Expr>> {
        for name in self.named.borrow().keys() {
            self.convert(name);
        }
        self.nameless.into_inner()
    }
}

fn make_nameless_env<'a>(env: Vec<Def<NamedExpr>>) -> HashMap<String, Def<Expr>> {
    LazyEnvConverter::new(env).into_nameless_env()
}

fn make_nameless_def<'a>(env: &'a LazyEnvConverter, def: &Def<NamedExpr>) -> Def<Expr> {
    let expr = make_nameless_expr(env, &def.params, &vec![], &def.expr);
    Def {
        name: def.name.clone(),
        arity: def.arity,
        params: def.params.clone(),
        expr,
    }
}

fn make_nameless_expr<'a>(
    env: &'a LazyEnvConverter,
    args: &[String],
    locals: &[String],
    expr: &NamedExpr,
) -> Expr {
    match expr {
        NamedExpr::Var(v) => Expr::Var(make_nameless_var(env, args, locals, &v)),
        NamedExpr::App(f, xs) => Expr::App(make_nameless_var(env, args, locals, &f), {
            let mut xs_nameless = vec![];
            for x in xs {
                xs_nameless.push(make_nameless_var(env, args, locals, &x))
            }
            xs_nameless.into_iter().collect()
        }),
        NamedExpr::Let(x, e1, e2) => {
            let mut body_locals = locals.to_vec();
            body_locals.push(x.clone());
            Expr::Let(
                Box::new(make_nameless_expr(env, args, locals, e1)),
                Box::new(make_nameless_expr(env, args, &body_locals, e2)),
            )
        }
        NamedExpr::Case(target, alts) => {
            let mut nameless_alts = vec![];
            for (Pat::Ctor(ctor, vars), rhs) in alts {
                let mut new_locals = locals.to_vec();
                for v in vars {
                    new_locals.push(v.clone());
                }
                nameless_alts.push((
                    Ctor { tag: ctor.tag },
                    make_nameless_expr(env, &args, &new_locals, rhs),
                ));
            }
            Expr::Case(make_nameless_var(env, args, locals, &target), nameless_alts)
        }
        NamedExpr::Ctor(ctor, xs) => {
            let mut nameless_xs = vec![];
            for x in xs {
                nameless_xs.push(make_nameless_var(env, args, locals, &x));
            }
            Expr::Ctor(Ctor { tag: ctor.tag }, nameless_xs)
        }
        NamedExpr::Int(n) => Expr::Int(*n),
        NamedExpr::Prim(p, xs) => {
            let mut nameless_xs = vec![];
            for x in xs {
                nameless_xs.push(make_nameless_var(env, args, locals, &x));
            }
            nameless_xs.reverse();

            Expr::Prim(*p, nameless_xs)
        }
    }
}

fn make_nameless_var<'a>(
    env: &'a LazyEnvConverter,
    args: &[String],
    locals: &[String],
    var: &NamedVar,
) -> Var {
    match var {
        NamedVar::Global(v) => Var::Global(v.clone()),
        NamedVar::Arg(v) => Var::Arg(args.iter().enumerate().find(|(_, a)| *a == v).unwrap().0),
        NamedVar::Local(v) => Var::Local(
            locals
                .iter()
                .rev()
                .enumerate()
                .find(|(_, a)| *a == v)
                .unwrap()
                .0,
        ),
    }
}

#[derive(Clone, Debug)]
enum Val<'a> {
    Ctor(Ctor, Vec<Rc<Val<'a>>>),
    Int(i32),
    PAp(&'a Def<Expr>, Vec<Rc<Val<'a>>>),
}

// A value that contains no partial applications
#[derive(Clone, Debug, PartialEq, Eq)]
enum DataVal {
    Ctor(Ctor, Vec<DataVal>),
    Int(i32),
}

fn val_to_data<'a>(val: &'a Val) -> DataVal {
    match val {
        Val::PAp(_, _) => panic!("Value contains partial application: {:?}", val),
        Val::Ctor(ctor, args) => {
            DataVal::Ctor(*ctor, args.into_iter().map(|a| val_to_data(a)).collect())
        }
        Val::Int(i) => DataVal::Int(*i),
    }
}

// Evaluation
// We evaluate nameless expressions
// We have two stacks: function args and local variables.
// Each stack element is a reference to a heap-allocated value: `Box<Val>`.
// Values are normal forms: ctors, ints, paps
// Evaluation returns a value
// I don't know if we need an explicit heap or not.

// Evaluation context
// This tells us where we are in the parent expression, so know what to do when we've finished
// evaluating some subexpression.
#[derive(Debug)]
enum Ctx<'a> {
    Eval(&'a Expr),
    EvalVar(Var),
    LetBind {
        body: &'a Expr,
    },
    LetDrop,
    AppEvalFunc {
        args: Vec<Var>,
    },
    AppCall {
        func: Rc<Val<'a>>,
        arg_vals: Vec<Rc<Val<'a>>>,
    },
    ArgDrop(usize),
    EvalVars0 {
        vars: Vec<Var>,
        callback: EvalVarsCallback<'a>,
    },
    EvalVarsFromResult {
        vars: Vec<Var>,
        vals: Vec<Rc<Val<'a>>>,
        callback: EvalVarsCallback<'a>,
    },
    EvalVars {
        vars: Vec<Var>,
        vals: Vec<Rc<Val<'a>>>,
        callback: EvalVarsCallback<'a>,
    },
    CtorFinish {
        ctor: Ctor,
        arg_vals: Vec<Rc<Val<'a>>>,
    },
    Case {
        alts: &'a [(Ctor, Expr)],
    },
    Prim {
        prim: Prim,
        arg_vals: Vec<Rc<Val<'a>>>,
    },
}

fn eval<'a>(
    env: &'a HashMap<String, Def<Expr>>,
    mut args: Stack<Rc<Val<'a>>>,
    mut locals: Stack<Rc<Val<'a>>>,
    start_expr: &'a Expr,
) -> DataVal {
    // Used to store the result of an evaluation
    // We initially set it to False/Nil/Nothing
    let mut result = Rc::new(Val::Ctor(Ctor { tag: 0 }, vec![]));
    // Used to store the parent context, when we have to evaluate sub-expressions.
    let mut ctx: Vec<Ctx> = vec![Ctx::Eval(start_expr)];

    loop {
        println!("ctx: {:?}", ctx);
        println!("args: {:?}\n", args);
        match ctx.pop() {
            None => {
                return val_to_data(&result);
            }
            // If a variable is global, it might be a nullary function that we need to evaluate.
            // If it's a local or argument, it is guaranteed to be in normal form already and we
            // can just push it onto the `vars` stack unchanged.
            Some(Ctx::EvalVars0 { mut vars, callback }) => match vars.pop() {
                Some(var) => {
                    let mut vals = vec![];
                    match lookup_var(env, &args, &locals, &var) {
                        VarLookup::NullaryDef(def) => {
                            ctx.push(Ctx::EvalVarsFromResult {
                                vars,
                                vals,
                                callback,
                            });
                            ctx.push(Ctx::Eval(&def.expr));
                        }
                        VarLookup::Val(v) => {
                            vals.push(v);
                            ctx.push(Ctx::EvalVars {
                                vars,
                                vals,
                                callback,
                            });
                        }
                    }
                }
                None => {
                    ctx.push(Ctx::EvalVars {
                        vars: vec![],
                        vals: vec![],
                        callback,
                    });
                }
            },
            Some(Ctx::EvalVarsFromResult {
                vars,
                mut vals,
                callback,
            }) => {
                vals.push(Rc::clone(&result));
                ctx.push(Ctx::EvalVars {
                    vars,
                    vals,
                    callback,
                });
            }
            Some(Ctx::EvalVars {
                mut vars,
                mut vals,
                callback,
            }) => match vars.pop() {
                Some(var) => match lookup_var(env, &args, &locals, &var) {
                    VarLookup::NullaryDef(def) => {
                        ctx.push(Ctx::EvalVarsFromResult {
                            vars,
                            vals,
                            callback,
                        });
                        ctx.push(Ctx::Eval(&def.expr));
                    }
                    VarLookup::Val(v) => {
                        vals.push(v);
                        ctx.push(Ctx::EvalVars {
                            vars,
                            vals,
                            callback,
                        });
                    }
                },
                None => {
                    ctx.push(callback.call(vals));
                }
            },
            // We've evaluated the function to a PAp and stored it in `result`.
            Some(Ctx::AppEvalFunc { args }) => {
                ctx.push(Ctx::EvalVars0 {
                    vars: args,
                    callback: EvalVarsCallback::MakeAppCall {
                        func: Rc::clone(&result),
                    },
                });
            }
            // We ensure that the args in a PAp are always in order, so first arg is index 0 etc.
            // The args on the arg stack are in stack-order, i.e. first arg is index 0 but the
            // indices count from the back of the vector.
            // So in reality the args on the arg stack are in reverse order.
            Some(Ctx::AppCall { func, mut arg_vals }) => {
                match func.as_ref() {
                    Val::PAp(def, existing_args) => {
                        // Check the arity of def
                        // If we have enough args, push them all onto the stack (even if we have too
                        // many).
                        // Then evaluate the def body
                        // If we don't have enough args, construct a PAp and store all the args in
                        // there.
                        let mut existing_args = existing_args.clone();
                        if def.arity <= existing_args.len() + arg_vals.len() {
                            // The new args are in reverse order, so unreverse them.
                            arg_vals.reverse();
                            // Then combine the existing args and the new args.
                            existing_args.append(&mut arg_vals);
                            // Then reverse them and push them onto the stack
                            existing_args.reverse();
                            args.push_chunk(existing_args);
                            // After we return from the function, drop each arg.
                            ctx.push(Ctx::ArgDrop(def.arity));
                            ctx.push(Ctx::Eval(&def.expr));
                        } else {
                            // `arg_vals` are in reverse order, so un-reverse them and add them
                            // to `existing_args`.
                            arg_vals.reverse();
                            existing_args.append(&mut arg_vals);
                            result = Rc::new(Val::PAp(def, existing_args));
                        }
                    }
                    _ => panic!("Application of non-function: {:?}", func),
                }
            }
            Some(Ctx::ArgDrop(n)) => {
                args.drop(n);
            }
            // `result` contains the evaluated bound value
            // Push it onto `locals` and evaluate the body
            Some(Ctx::LetBind { body }) => {
                locals.push(Rc::clone(&result));
                ctx.push(Ctx::LetDrop);
                ctx.push(Ctx::Eval(body))
            }
            Some(Ctx::LetDrop) => {
                locals.pop();
            }
            Some(Ctx::CtorFinish { ctor, mut arg_vals }) => {
                // `arg_vals` is reversed by EvalVars, so we need to unreverse it
                arg_vals.reverse();
                result = Rc::new(Val::Ctor(ctor, arg_vals));
            }
            Some(Ctx::EvalVar(var)) => match lookup_var(env, &args, &locals, &var) {
                VarLookup::Val(v) => {
                    result = v;
                }
                VarLookup::NullaryDef(def) => {
                    ctx.push(Ctx::Eval(&def.expr));
                }
            },
            Some(Ctx::Case { alts }) => {
                let target = Rc::clone(&result);
                // Examine target, check tag, eval alt with same tag, binding ctor args
                // TODO: default alt
                // TODO: order alts by tag and jump to corresponding index
                match target.as_ref() {
                    Val::Ctor(ctor, ctor_args) => {
                        match alts.iter().find(|(alt_ctor, _)| *alt_ctor == *ctor) {
                            None => panic!("No matching case alternative for {:?}", result),
                            Some((_, rhs)) => {
                                // Drop ctor arg bindings after eval
                                for _ in ctor_args {
                                    ctx.push(Ctx::LetDrop);
                                }
                                // Push ctor_args onto locals stack
                                // TODO: what order?
                                locals.push_chunk(ctor_args.clone());
                                // Eval rhs
                                ctx.push(Ctx::Eval(&rhs))
                            }
                        }
                    }
                    _ => panic!("Case analysis on non-constructor {:?}", target),
                }
            }
            Some(Ctx::Prim { prim, arg_vals }) => {
                result = match (&prim, &arg_vals[..]) {
                    (Prim::IntAdd, [xref, yref]) => match (xref.as_ref(), yref.as_ref()) {
                        (&Val::Int(x), &Val::Int(y)) => Rc::new(Val::Int(x + y)),
                        _ => panic!("prim + applied to bad args: {:?}", arg_vals),
                    },
                    (Prim::IntSub, [xref, yref]) => match (xref.as_ref(), yref.as_ref()) {
                        (&Val::Int(x), &Val::Int(y)) => Rc::new(Val::Int(x - y)),
                        _ => panic!("prim - applied to bad args: {:?}", arg_vals),
                    },
                    (Prim::IntEq, [xref, yref]) => match (xref.as_ref(), yref.as_ref()) {
                        (&Val::Int(x), &Val::Int(y)) => Rc::new(Val::Ctor(
                            Ctor {
                                tag: (x == y) as usize,
                            },
                            vec![],
                        )),
                        _ => panic!("prim ==(int) applied to bad args: {:?}", arg_vals),
                    },
                    (Prim::IntLt, [xref, yref]) => match (xref.as_ref(), yref.as_ref()) {
                        (&Val::Int(x), &Val::Int(y)) => Rc::new(Val::Ctor(
                            Ctor {
                                tag: (x < y) as usize,
                            },
                            vec![],
                        )),
                        _ => panic!("prim ==(int) applied to bad args: {:?}", arg_vals),
                    },
                    _ => panic!("prim {:?} applied to bad args: {:?}", prim, arg_vals),
                };
            }
            Some(Ctx::Eval(expr)) => {
                match expr {
                    Expr::Int(n) => {
                        result = Rc::new(Val::Int(*n));
                    }
                    Expr::Var(v) => {
                        ctx.push(Ctx::EvalVar(v.clone()));
                    }
                    Expr::App(f, xs) => match lookup_var(env, &args, &locals, &f) {
                        VarLookup::NullaryDef(def) => {
                            ctx.push(Ctx::Eval(&def.expr));
                        }
                        VarLookup::Val(v) => {
                            result = Rc::clone(&v);
                            ctx.push(Ctx::AppEvalFunc { args: xs.to_vec() })
                        }
                    },
                    Expr::Let(e1, e2) => {
                        // Evaluate the bound value first
                        ctx.push(Ctx::LetBind { body: e2 });
                        ctx.push(Ctx::Eval(e1));
                    }
                    Expr::Ctor(ctor, vars) => {
                        if vars.is_empty() {
                            result = Rc::new(Val::Ctor(*ctor, vec![]));
                        } else {
                            ctx.push(Ctx::EvalVars0 {
                                vars: vars.clone(),
                                callback: EvalVarsCallback::MakeCtorFinish { ctor: *ctor },
                            });
                        }
                    }
                    Expr::Case(target, alts) => {
                        ctx.push(Ctx::Case { alts: &alts });
                        ctx.push(Ctx::EvalVar(target.clone()));
                    }
                    Expr::Prim(prim, vars) => {
                        ctx.push(Ctx::EvalVars0 {
                            vars: vars.clone(),
                            callback: EvalVarsCallback::MakePrim { prim: *prim },
                        });
                    }
                }
            }
        }
    }
}

// A callback for EvalVars
// This should take the values and produce a new Ctx
#[derive(Debug)]
enum EvalVarsCallback<'a> {
    MakeAppCall { func: Rc<Val<'a>> },
    MakeCtorFinish { ctor: Ctor },
    MakePrim { prim: Prim },
}

impl<'a> EvalVarsCallback<'a> {
    fn call(self, arg_vals: Vec<Rc<Val<'a>>>) -> Ctx<'a> {
        match self {
            EvalVarsCallback::MakeAppCall { func } => Ctx::AppCall { func, arg_vals },
            EvalVarsCallback::MakeCtorFinish { ctor } => Ctx::CtorFinish { ctor, arg_vals },
            EvalVarsCallback::MakePrim { prim } => Ctx::Prim { prim, arg_vals },
        }
    }
}

#[derive(Debug)]
enum VarLookup<'a> {
    NullaryDef(&'a Def<Expr>),
    Val(Rc<Val<'a>>),
}

fn lookup_var<'a, 'b>(
    env: &'a HashMap<String, Def<Expr>>,
    args: &'b Stack<Rc<Val<'a>>>,
    locals: &'b Stack<Rc<Val<'a>>>,
    var: &'b Var,
) -> VarLookup<'a> {
    match var {
        Var::Global(name) => match env.get(name) {
            Some(def) if def.arity == 0 => VarLookup::NullaryDef(def),
            Some(def) => VarLookup::Val(Rc::new(Val::PAp(def, vec![]))),
            None => panic!("Unknown global variable {}", name),
        },

        Var::Local(i) => VarLookup::Val(locals[*i].clone()),
        Var::Arg(i) => VarLookup::Val(args[*i].clone()),
    }
}

// Why do we use the same stack layout for both locals and args?
// It's because we have to keep arguments around even when entering a new function, because we'll
// eventually return from that function into the parent and it may need to access its args.
// So we can't construct a new args stack for each function call.
// So it's most efficient to just add args for a new function call onto the end of the existing
// args stack. This is the same layout we use for locals.
#[derive(Debug)]
struct Stack<T> {
    inner: Vec<T>,
}

impl<T> Stack<T> {
    fn new() -> Self {
        Stack { inner: vec![] }
    }
    fn push(&mut self, x: T) {
        self.inner.push(x);
    }
    fn push_chunk(&mut self, mut xs: Vec<T>) {
        self.inner.append(&mut xs);
    }
    fn pop(&mut self) {
        self.inner.pop();
    }
    fn drop(&mut self, n: usize) {
        self.inner.truncate(self.inner.len() - n);
    }
}

impl<T> std::ops::Index<usize> for Stack<T> {
    type Output = T;
    fn index(&self, index: usize) -> &T {
        let i = self.inner.len() - 1 - index;
        &self.inner[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[cfg(test)]
    use pretty_assertions::assert_eq;

    fn run_eval_test(defs: Vec<Def<NamedExpr>>) -> DataVal {
        let env = make_nameless_env(defs);
        eval(
            &env,
            Stack::new(),
            Stack::new(),
            &env.get("main").unwrap().expr,
        )
    }
    #[test]
    fn test_1() {
        let unit_ctor = NamedCtor {
            name: "Unit".into(),
            tag: 0,
        };
        let nil_ctor = NamedCtor {
            name: "Nil".into(),
            tag: 0,
        };
        let cons_ctor = NamedCtor {
            name: "Cons".into(),
            tag: 1,
        };
        let unit: NamedExpr = NamedExpr::Ctor(unit_ctor, vec![]);
        let nil: NamedExpr = NamedExpr::Ctor(nil_ctor, vec![]);

        // let nil = Nil
        //  in let unit = Unit
        //      in Cons unit nil
        let example_named = NamedExpr::Let(
            "nil".into(),
            Box::new(nil.clone()),
            Box::new(NamedExpr::Let(
                "unit".into(),
                Box::new(unit.clone()),
                Box::new(NamedExpr::Ctor(
                    cons_ctor.clone(),
                    vec![
                        NamedVar::Local("unit".into()),
                        NamedVar::Local("nil".into()),
                    ],
                )),
            )),
        );
        let env = LazyEnvConverter::new(vec![]);
        let example_nameless = make_nameless_expr(&env, &vec![], &vec![], &example_named);
        assert_eq!(
            example_nameless,
            Expr::Let(
                Box::new(Expr::Ctor(Ctor { tag: 0 }, vec![])),
                Box::new(Expr::Let(
                    Box::new(Expr::Ctor(Ctor { tag: 0 }, vec![])),
                    Box::new(Expr::Ctor(
                        Ctor { tag: 1 },
                        vec![Var::Local(0), Var::Local(1)]
                    ))
                ))
            )
        );
        let result = eval(
            &env.into_nameless_env(),
            Stack::new(),
            Stack::new(),
            &example_nameless,
        );
        assert_eq!(
            result,
            DataVal::Ctor(
                Ctor { tag: 1 },
                vec![
                    DataVal::Ctor(Ctor { tag: 0 }, vec![]),
                    DataVal::Ctor(Ctor { tag: 0 }, vec![])
                ]
            )
        );
    }

    #[test]
    fn test_2() {
        let id = Def {
            name: "id".into(),
            arity: 1,
            params: vec!["x".into()],
            expr: NamedExpr::Var(NamedVar::Arg("x".into())),
        };
        let unit = Def {
            name: "unit".into(),
            arity: 0,
            params: vec![],
            expr: NamedExpr::Ctor(
                NamedCtor {
                    name: "Unit".into(),
                    tag: 0,
                },
                vec![],
            ),
        };
        let main = Def {
            name: "main".into(),
            arity: 0,
            params: vec![],
            expr: NamedExpr::App(
                NamedVar::Global("id".into()),
                vec![NamedVar::Global("unit".into())],
            ),
        };

        let env = make_nameless_env(vec![id, unit, main]);

        assert_eq!(env.get("id").unwrap().expr, Expr::Var(Var::Arg(0)));
        assert_eq!(
            env.get("unit").unwrap().expr,
            Expr::Ctor(Ctor { tag: 0 }, vec![])
        );
        assert_eq!(
            env.get("main").unwrap().expr,
            Expr::App(Var::Global("id".into()), vec![Var::Global("unit".into())])
        );

        let result = eval(
            &env,
            Stack::new(),
            Stack::new(),
            &env.get("main").unwrap().expr,
        );

        assert_eq!(result, DataVal::Ctor(Ctor { tag: 0 }, vec![]));
    }

    #[test]
    fn test_3() {
        let true_ctor = NamedCtor {
            name: "True".into(),
            tag: 1,
        };
        let false_ctor = NamedCtor {
            name: "False".into(),
            tag: 0,
        };
        let not = Def {
            name: "not".into(),
            arity: 1,
            params: vec!["b".into()],
            expr: NamedExpr::Case(
                NamedVar::Arg("b".into()),
                vec![
                    (
                        Pat::Ctor(true_ctor.clone(), vec![]),
                        NamedExpr::Ctor(false_ctor.clone(), vec![]),
                    ),
                    (
                        Pat::Ctor(false_ctor.clone(), vec![]),
                        NamedExpr::Ctor(true_ctor.clone(), vec![]),
                    ),
                ],
            ),
        };
        let main = Def {
            name: "main".into(),
            arity: 0,
            params: vec![],
            expr: NamedExpr::Let(
                "false".into(),
                Box::new(NamedExpr::Ctor(false_ctor.clone(), vec![])),
                Box::new(NamedExpr::App(
                    NamedVar::Global("not".into()),
                    vec![NamedVar::Local("false".into())],
                )),
            ),
        };

        let env = make_nameless_env(vec![not, main]);

        assert_eq!(
            env.get("not").unwrap().expr,
            Expr::Case(
                Var::Arg(0),
                vec![
                    (Ctor { tag: 1 }, Expr::Ctor(Ctor { tag: 0 }, vec![])),
                    (Ctor { tag: 0 }, Expr::Ctor(Ctor { tag: 1 }, vec![]))
                ]
            )
        );
        assert_eq!(
            env.get("main").unwrap().expr,
            Expr::Let(
                Box::new(Expr::Ctor(Ctor { tag: 0 }, vec![])),
                Box::new(Expr::App(Var::Global("not".into()), vec![Var::Local(0)]))
            )
        );

        let result = eval(
            &env,
            Stack::new(),
            Stack::new(),
            &env.get("main").unwrap().expr,
        );

        assert_eq!(result, DataVal::Ctor(Ctor { tag: 1 }, vec![]));
    }

    #[test]
    fn test_4() {
        // fromMaybe(m, d) = case m of { Just x -> x; Nothing -> d }
        // main = let one = 1 in let two = 2 in let r = Just two in fromMaybe r one

        let just_ctor = NamedCtor {
            name: "Just".into(),
            tag: 1,
        };
        let nothing_ctor = NamedCtor {
            name: "Nothing".into(),
            tag: 0,
        };
        let from_maybe = Def {
            name: "fromMaybe".into(),
            arity: 1,
            params: vec!["m".into(), "d".into()],
            expr: NamedExpr::Case(
                NamedVar::Arg("m".into()),
                vec![
                    (
                        Pat::Ctor(just_ctor.clone(), vec!["x".into()]),
                        NamedExpr::Var(NamedVar::Local("x".into())),
                    ),
                    (
                        Pat::Ctor(nothing_ctor.clone(), vec![]),
                        NamedExpr::Var(NamedVar::Arg("d".into())),
                    ),
                ],
            ),
        };
        let main = Def {
            name: "main".into(),
            arity: 0,
            params: vec![],
            expr: NamedExpr::Let(
                "one".into(),
                Box::new(NamedExpr::Int(1)),
                Box::new(NamedExpr::Let(
                    "two".into(),
                    Box::new(NamedExpr::Int(2)),
                    Box::new(NamedExpr::Let(
                        "r".into(),
                        Box::new(NamedExpr::Ctor(
                            just_ctor,
                            vec![NamedVar::Local("two".into())],
                        )),
                        Box::new(NamedExpr::App(
                            NamedVar::Global("fromMaybe".into()),
                            vec![NamedVar::Local("r".into()), NamedVar::Local("one".into())],
                        )),
                    )),
                )),
            ),
        };

        let env = make_nameless_env(vec![from_maybe, main]);

        assert_eq!(
            env.get("fromMaybe").unwrap().expr,
            Expr::Case(
                Var::Arg(0),
                vec![
                    (Ctor { tag: 1 }, Expr::Var(Var::Local(0)),),
                    (Ctor { tag: 0 }, Expr::Var(Var::Arg(1)),),
                ],
            ),
        );
        assert_eq!(
            env.get("main").unwrap().expr,
            Expr::Let(
                Box::new(Expr::Int(1)),
                Box::new(Expr::Let(
                    Box::new(Expr::Int(2)),
                    Box::new(Expr::Let(
                        Box::new(Expr::Ctor(Ctor { tag: 1 }, vec![Var::Local(0)],)),
                        Box::new(Expr::App(
                            Var::Global("fromMaybe".into()),
                            vec![Var::Local(0), Var::Local(2)],
                        )),
                    )),
                )),
            ),
        );

        let result = eval(
            &env,
            Stack::new(),
            Stack::new(),
            &env.get("main").unwrap().expr,
        );

        assert_eq!(result, DataVal::Int(2));
    }

    fn map_example() -> Def<NamedExpr> {
        Def {
            name: "map".into(),
            arity: 2,
            params: vec!["f".into(), "l".into()],
            expr: NamedExpr::Case(
                NamedVar::Arg("l".into()),
                vec![
                    (
                        Pat::Ctor(
                            NamedCtor {
                                name: "Nil".into(),
                                tag: 0,
                            },
                            vec![],
                        ),
                        NamedExpr::Ctor(
                            NamedCtor {
                                name: "Nil".into(),
                                tag: 0,
                            },
                            vec![],
                        ),
                    ),
                    (
                        Pat::Ctor(
                            NamedCtor {
                                name: "Cons".into(),
                                tag: 1,
                            },
                            vec!["x".into(), "xs".into()],
                        ),
                        NamedExpr::Let(
                            "x'".into(),
                            Box::new(NamedExpr::App(
                                NamedVar::Arg("f".into()),
                                vec![NamedVar::Local("x".into())],
                            )),
                            Box::new(NamedExpr::Let(
                                "xs'".into(),
                                Box::new(NamedExpr::App(
                                    NamedVar::Global("map".into()),
                                    vec![NamedVar::Arg("f".into()), NamedVar::Local("xs".into())],
                                )),
                                Box::new(NamedExpr::Ctor(
                                    NamedCtor {
                                        name: "Cons".into(),
                                        tag: 1,
                                    },
                                    vec![
                                        NamedVar::Local("x'".into()),
                                        NamedVar::Local("xs'".into()),
                                    ],
                                )),
                            )),
                        ),
                    ),
                ],
            ),
        }
    }

    #[test]
    fn test_5() {
        let false_ctor = NamedCtor {
            name: "False".into(),
            tag: 0,
        };
        let true_ctor = NamedCtor {
            name: "True".into(),
            tag: 1,
        };
        let nil_ctor = NamedCtor {
            name: "Nil".into(),
            tag: 0,
        };
        let cons_ctor = NamedCtor {
            name: "Cons".into(),
            tag: 1,
        };
        let not = Def {
            name: "not".into(),
            arity: 1,
            params: vec!["b".into()],
            expr: NamedExpr::Case(
                NamedVar::Arg("b".into()),
                vec![
                    (
                        Pat::Ctor(true_ctor.clone(), vec![]),
                        NamedExpr::Ctor(false_ctor.clone(), vec![]),
                    ),
                    (
                        Pat::Ctor(false_ctor.clone(), vec![]),
                        NamedExpr::Ctor(true_ctor.clone(), vec![]),
                    ),
                ],
            ),
        };
        let main = Def {
            name: "main".into(),
            arity: 0,
            params: vec![],
            expr: NamedExpr::Let(
                "nil".into(),
                Box::new(NamedExpr::Ctor(nil_ctor.clone(), vec![])),
                Box::new(NamedExpr::Let(
                    "true".into(),
                    Box::new(NamedExpr::Ctor(true_ctor.clone(), vec![])),
                    Box::new(NamedExpr::Let(
                        "false".into(),
                        Box::new(NamedExpr::Ctor(false_ctor.clone(), vec![])),
                        Box::new(NamedExpr::Let(
                            "l1".into(),
                            Box::new(NamedExpr::Ctor(
                                cons_ctor.clone(),
                                vec![
                                    NamedVar::Local("false".into()),
                                    NamedVar::Local("nil".into()),
                                ],
                            )),
                            Box::new(NamedExpr::Let(
                                "l2".into(),
                                Box::new(NamedExpr::Ctor(
                                    cons_ctor.clone(),
                                    vec![
                                        NamedVar::Local("true".into()),
                                        NamedVar::Local("l1".into()),
                                    ],
                                )),
                                Box::new(NamedExpr::App(
                                    NamedVar::Global("map".into()),
                                    vec![
                                        NamedVar::Global("not".into()),
                                        NamedVar::Local("l2".into()),
                                    ],
                                )),
                            )),
                        )),
                    )),
                )),
            ),
        };

        let env = make_nameless_env(vec![not.clone(), map_example(), main.clone()]);

        assert_eq!(
            env.get("map").unwrap().expr,
            Expr::Case(
                Var::Arg(1),
                vec![
                    (Ctor { tag: 0 }, Expr::Ctor(Ctor { tag: 0 }, vec![])),
                    (
                        Ctor { tag: 1 },
                        Expr::Let(
                            Box::new(Expr::App(Var::Arg(0), vec![Var::Local(1)])),
                            Box::new(Expr::Let(
                                Box::new(Expr::App(
                                    Var::Global("map".into()),
                                    vec![Var::Arg(0), Var::Local(1)]
                                )),
                                Box::new(Expr::Ctor(
                                    Ctor { tag: 1 },
                                    vec![Var::Local(1), Var::Local(0)]
                                ))
                            ))
                        )
                    )
                ],
            ),
        );

        assert_eq!(
            env.get("main").unwrap().expr,
            Expr::Let(
                Box::new(Expr::Ctor(Ctor { tag: 0 }, vec![])),
                Box::new(Expr::Let(
                    Box::new(Expr::Ctor(Ctor { tag: 1 }, vec![])),
                    Box::new(Expr::Let(
                        Box::new(Expr::Ctor(Ctor { tag: 0 }, vec![])),
                        Box::new(Expr::Let(
                            Box::new(Expr::Ctor(
                                Ctor { tag: 1 },
                                vec![Var::Local(0), Var::Local(2)]
                            )),
                            Box::new(Expr::Let(
                                Box::new(Expr::Ctor(
                                    Ctor { tag: 1 },
                                    vec![Var::Local(2), Var::Local(0)]
                                )),
                                Box::new(Expr::App(
                                    Var::Global("map".into()),
                                    vec![Var::Global("not".into()), Var::Local(0)]
                                ))
                            ))
                        ))
                    ))
                ))
            )
        );

        assert_eq!(
            run_eval_test(vec![not, map_example(), main]),
            DataVal::Ctor(
                Ctor { tag: 1 },
                vec![
                    DataVal::Ctor(Ctor { tag: 0 }, vec![]),
                    DataVal::Ctor(
                        Ctor { tag: 1 },
                        vec![
                            DataVal::Ctor(Ctor { tag: 1 }, vec![]),
                            DataVal::Ctor(Ctor { tag: 0 }, vec![])
                        ]
                    )
                ]
            )
        );
    }
}

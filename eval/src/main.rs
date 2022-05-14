use std::rc::Rc;

fn main() {
    println!("Hello, world!");
}

enum NamedExpr {
    Var(NamedVar),
    App(NamedVar, Vec<NamedVar>),
    Let(String, Box<NamedExpr>, Box<NamedExpr>),
    Case(NamedVar, Vec<(Pat, Box<NamedExpr>)>),
    Ctor(NamedCtor, Vec<NamedVar>),
    Int(i32),
    Prim(Prim, Vec<NamedVar>),
}

enum Pat {
    Ctor(NamedCtor, Vec<String>),
}

enum NamedVar {
    Global(String),
    Local(String),
    Arg(String),
}

struct NamedCtor {
    name: String,
    tag: usize,
}

#[derive(Debug)]
enum Prim {
    IntAdd,
    IntSub,
    IntEq,
    IntLt,
}

// A nameless expression.
// Variables are indices.
#[derive(Debug)]
enum Expr<'a> {
    Var(Var<'a>),
    App(Var<'a>, Vec<Var<'a>>),
    Let(Box<Expr<'a>>, Box<Expr<'a>>),
    Case(Var<'a>, Vec<(Ctor, Box<Expr<'a>>)>),
    Ctor(Ctor, Vec<Var<'a>>),
    Int(i32),
    Prim(Prim, Vec<Var<'a>>),
}

#[derive(Clone, Debug)]
enum Var<'a> {
    Global(&'a Def<Expr<'a>>),
    Local(usize),
    Arg(usize),
}

#[derive(Copy, Clone, Debug)]
struct Ctor {
    tag: usize,
}

#[derive(Debug)]
struct Def<E> {
    name: String,
    arity: usize,
    params: Vec<String>,
    expr: E,
}

struct Env<E>(Vec<Def<E>>);

// consumes `def`
fn make_nameless_def<'a>(env: &'a Env<Expr<'a>>, def: Def<NamedExpr>) -> Def<Expr<'a>> {
    let expr = make_nameless_expr(env, &def.params, &vec![], def.expr);
    Def {
        name: def.name,
        arity: def.arity,
        params: def.params,
        expr,
    }
}

// consumes `expr`
fn make_nameless_expr<'a>(
    env: &'a Env<Expr<'a>>,
    args: &[String],
    locals: &[String],
    expr: NamedExpr,
) -> Expr<'a> {
    match expr {
        NamedExpr::Var(v) => Expr::Var(make_nameless_var(env, args, locals, &v)),
        NamedExpr::App(f, xs) => Expr::App(
            make_nameless_var(env, args, locals, &f),
            xs.iter()
                .map(|x| make_nameless_var(env, args, locals, x))
                .collect(),
        ),
        NamedExpr::Let(x, e1, e2) => {
            let mut body_locals = locals.to_vec();
            body_locals.push(x);
            Expr::Let(
                Box::new(make_nameless_expr(env, args, locals, *e1)),
                Box::new(make_nameless_expr(env, args, &body_locals, *e2)),
            )
        }
        NamedExpr::Case(target, alts) => Expr::Case(
            make_nameless_var(env, args, locals, &target),
            alts.into_iter()
                .map(|(Pat::Ctor(ctor, vars), rhs)| {
                    let mut new_locals = locals.to_vec();
                    for v in vars {
                        new_locals.push(v);
                    }
                    (
                        Ctor { tag: ctor.tag },
                        Box::new(make_nameless_expr(env, &args, &new_locals, *rhs)),
                    )
                })
                .collect(),
        ),
        NamedExpr::Ctor(ctor, xs) => Expr::Ctor(
            Ctor { tag: ctor.tag },
            xs.into_iter()
                .map(|x| make_nameless_var(env, args, locals, &x))
                .collect(),
        ),
        NamedExpr::Int(n) => Expr::Int(n),
        NamedExpr::Prim(p, xs) => Expr::Prim(
            p,
            xs.into_iter()
                .map(|x| make_nameless_var(env, args, locals, &x))
                .collect(),
        ),
    }
}

fn make_nameless_var<'a>(
    env: &'a Env<Expr<'a>>,
    args: &[String],
    locals: &[String],
    var: &NamedVar,
) -> Var<'a> {
    match var {
        NamedVar::Global(v) => Var::Global(env.0.iter().find(|d| d.name == *v).unwrap()),
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
    PAp(&'a Def<Expr<'a>>, Vec<Rc<Val<'a>>>),
}

// A value that contains no partial applications
enum DataVal {
    Ctor(Ctor, Vec<DataVal>),
    Int(i32),
}

fn val_to_data<'a>(val: &'a Val<'a>) -> DataVal {
    match val {
        Val::PAp(_, _) => panic!("Value contains partial application"),
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
enum Ctx<'a> {
    Eval(&'a Expr<'a>),
    LetBind {
        body: &'a Expr<'a>,
    },
    AppEvalFunc {
        args: Vec<Var<'a>>,
    },
    AppEvalArg {
        func: Rc<Val<'a>>,
        arg_vars: Vec<Var<'a>>,
        arg_vals: Vec<Rc<Val<'a>>>,
    },
    AppPushArg {
        func: Rc<Val<'a>>,
        arg_vars: Vec<Var<'a>>,
        arg_vals: Vec<Rc<Val<'a>>>,
    },
}

fn eval<'a>(
    mut args: Stack<Rc<Val<'a>>>,
    mut locals: Stack<Rc<Val<'a>>>,
    start_expr: &'a Expr<'a>,
) -> DataVal {
    let mut result = Rc::new(Val::Ctor(Ctor { tag: 0 }, vec![]));
    let mut ctx: Vec<Ctx> = vec![Ctx::Eval(start_expr)];

    loop {
        match ctx.pop() {
            None => {
                return val_to_data(&result);
            }
            // We've evaluated the function to a PAp and stored it in `result`.
            Some(Ctx::AppEvalFunc { args }) => {
                ctx.push(Ctx::AppEvalArg {
                    func: result.clone(),
                    arg_vars: args,
                    arg_vals: vec![],
                });
            }
            // We've just evaluated an argument and it is in `result`, so push it onto `arg_vals`.
            Some(Ctx::AppPushArg {
                func,
                arg_vars,
                mut arg_vals,
            }) => {
                arg_vals.push(result.clone());
                ctx.push(Ctx::AppEvalArg {
                    func,
                    arg_vars,
                    arg_vals,
                });
            }
            Some(Ctx::AppEvalArg {
                func,
                mut arg_vars,
                mut arg_vals,
            }) => {
                // We've evaluated one of the arguments and stored it in `result`.
                // `arg_vars` contains the remaining unevaluated arguments.
                // If there's an arg left in `arg_vars`, we evaluate it and loop.
                if let Some(arg) = arg_vars.pop() {
                    match arg {
                        Var::Local(i) => {
                            arg_vals.push(locals[i].clone());
                            ctx.push(Ctx::AppEvalArg {
                                func,
                                arg_vars,
                                arg_vals,
                            })
                        }
                        Var::Arg(i) => {
                            arg_vals.push(args[i].clone());
                            ctx.push(Ctx::AppEvalArg {
                                func,
                                arg_vars,
                                arg_vals,
                            })
                        }
                        Var::Global(def) => {
                            if def.arity == 0 {
                                ctx.push(Ctx::AppPushArg {
                                    func,
                                    arg_vars,
                                    arg_vals,
                                });
                                ctx.push(Ctx::Eval(&def.expr));
                            } else {
                                // This will get pushed onto `arg_vals` on the next loop
                                arg_vals.push(Rc::new(Val::PAp(def, vec![])));
                                ctx.push(Ctx::AppEvalArg {
                                    func,
                                    arg_vars,
                                    arg_vals,
                                });
                            }
                        }
                    }
                }
                // If not, we can move to the next step.
                else {
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
                                // Args are pushed onto the stack so the first arg is at the top,
                                // but `arg_vals` is already reversed so we can just push it on.
                                args.push_chunk(arg_vals);
                                // The existing args need to be reversed before pushing.
                                existing_args.reverse();
                                args.push_chunk(existing_args);
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
            }
            // `result` contains the evaluated bound value
            // Push it onto `locals` and evaluate the body
            Some(Ctx::LetBind { body }) => {
                locals.push(result.clone());
                ctx.push(Ctx::Eval(body))
            }
            Some(Ctx::Eval(expr)) => {
                match expr {
                    Expr::Int(n) => {
                        result = Rc::new(Val::Int(*n));
                    }
                    Expr::Var(v) => match v {
                        Var::Local(i) => {
                            result = locals[*i].clone();
                        }
                        Var::Arg(i) => {
                            result = args[*i].clone();
                        }
                        Var::Global(def) => {
                            if def.arity == 0 {
                                // If the function is nullary, jump straight to its body.
                                ctx.push(Ctx::Eval(&def.expr));
                            } else {
                                result = Rc::new(Val::PAp(def, vec![]));
                            }
                        }
                    },
                    Expr::App(f, xs) => {
                        match f {
                            Var::Local(i) => {
                                result = locals[*i].clone();
                                ctx.push(Ctx::AppEvalFunc { args: xs.to_vec() })
                            }
                            Var::Arg(i) => {
                                result = args[*i].clone();
                                ctx.push(Ctx::AppEvalFunc { args: xs.to_vec() })
                            }
                            Var::Global(def) => {
                                if def.arity == 0 {
                                    // If the function is nullary, jump straight to its body.
                                    ctx.push(Ctx::Eval(&def.expr));
                                } else {
                                    result = Rc::new(Val::PAp(def, vec![]));
                                    ctx.push(Ctx::AppEvalFunc { args: xs.to_vec() })
                                }
                            }
                        };
                    }
                    Expr::Let(e1, e2) => {
                        // Evaluate the bound value first
                        ctx.push(Ctx::LetBind { body: e2 });
                        ctx.push(Ctx::Eval(e1.clone()));
                    }
                }
            }
        }
    }
}

enum VarLookup<'a> {
    Def(&'a Def<Expr<'a>>),
    Val(Rc<Val<'a>>),
}

fn lookup_var2<'a>(
    args: &'a Stack<Rc<Val<'a>>>,
    locals: &'a Stack<Rc<Val<'a>>>,
    var: &Var<'a>,
) -> VarLookup<'a> {
    match var {
        Var::Global(def) => VarLookup::Def(def),
        Var::Local(i) => VarLookup::Val(locals[*i].clone()),
        Var::Arg(i) => VarLookup::Val(args[*i].clone()),
    }
}

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
}

impl<T> std::ops::Index<usize> for Stack<T> {
    type Output = T;
    fn index(&self, index: usize) -> &T {
        let i = self.inner.len() - 1 - index;
        &self.inner[i]
    }
}

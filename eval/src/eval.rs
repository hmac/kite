use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::AtomicUsize;

#[derive(Clone, Debug)]
pub enum NamedExpr {
    Var(NamedVar),
    App(NamedVar, Vec<NamedVar>),
    Let(String, Box<NamedExpr>, Box<NamedExpr>),
    Case(NamedVar, Vec<(Pat, NamedExpr)>),
    Ctor(NamedCtor, Vec<NamedVar>),
    Int(i32),
    Prim(Prim, Vec<NamedVar>),
    Inc(NamedVar, Box<NamedExpr>),
    Dec(NamedVar, Box<NamedExpr>),
}

#[derive(Clone, Debug)]
pub enum Pat {
    Ctor(NamedCtor, Vec<String>),
}

#[derive(Clone, Debug)]
pub enum NamedVar {
    Global(String),
    Local(String),
    Arg(String),
}

#[derive(Clone, Debug)]
pub struct NamedCtor {
    // This field isn't used for evaluation - it's just a useful hint when writing tests
    pub name: String,
    pub tag: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Prim {
    IntAdd,
    IntSub,
    IntEq,
    IntLt,
    Panic,
}

// A nameless expression.
// Variables are indices.
#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Var(Var),
    App(Var, Vec<Var>),
    Let(Box<Expr>, Box<Expr>),
    Case(Var, Vec<(Ctor, Expr)>),
    Ctor(Ctor, Vec<Var>),
    Int(i32),
    Prim(Prim, Vec<Var>),
    Inc(Var, Box<Expr>),
    Dec(Var, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Var {
    // TODO: this is slow; store reference to Def instead
    Global(String),
    Local(usize),
    Arg(usize),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Ctor {
    pub tag: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Def<E> {
    pub name: String,
    pub arity: usize,
    pub params: Vec<String>,
    pub expr: E,
}

pub struct Env<E>(Vec<Def<E>>);

pub fn make_nameless_env(env: Vec<Def<NamedExpr>>) -> HashMap<String, Def<Expr>> {
    let mut map = HashMap::new();
    map.extend(env.into_iter().map(|def| {
        let nameless = make_nameless_def(&def);
        (def.name, nameless)
    }));
    map
}

fn make_nameless_def(def: &Def<NamedExpr>) -> Def<Expr> {
    let expr = make_nameless_expr(&def.params, &[], &def.expr);
    Def {
        name: def.name.clone(),
        arity: def.arity,
        params: def.params.clone(),
        expr,
    }
}

fn make_nameless_expr(args: &[String], locals: &[String], expr: &NamedExpr) -> Expr {
    match expr {
        NamedExpr::Var(v) => Expr::Var(make_nameless_var(args, locals, v)),
        NamedExpr::App(f, xs) => Expr::App(make_nameless_var(args, locals, f), {
            let mut xs_nameless = vec![];
            for x in xs {
                xs_nameless.push(make_nameless_var(args, locals, x))
            }
            xs_nameless.into_iter().collect()
        }),
        NamedExpr::Let(x, e1, e2) => {
            let mut body_locals = locals.to_vec();
            body_locals.push(x.clone());
            Expr::Let(
                Box::new(make_nameless_expr(args, locals, e1)),
                Box::new(make_nameless_expr(args, &body_locals, e2)),
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
                    make_nameless_expr(args, &new_locals, rhs),
                ));
            }
            Expr::Case(make_nameless_var(args, locals, target), nameless_alts)
        }
        NamedExpr::Ctor(ctor, xs) => {
            let mut nameless_xs = vec![];
            for x in xs {
                nameless_xs.push(make_nameless_var(args, locals, x));
            }
            Expr::Ctor(Ctor { tag: ctor.tag }, nameless_xs)
        }
        NamedExpr::Int(n) => Expr::Int(*n),
        NamedExpr::Prim(p, xs) => {
            let mut nameless_xs = vec![];
            for x in xs {
                nameless_xs.push(make_nameless_var(args, locals, x));
            }
            nameless_xs.reverse();

            Expr::Prim(*p, nameless_xs)
        }
        NamedExpr::Inc(x, e) => Expr::Inc(
            make_nameless_var(args, locals, x),
            Box::new(make_nameless_expr(args, locals, e)),
        ),
        NamedExpr::Dec(x, e) => Expr::Dec(
            make_nameless_var(args, locals, x),
            Box::new(make_nameless_expr(args, locals, e)),
        ),
    }
}

fn make_nameless_var(args: &[String], locals: &[String], var: &NamedVar) -> Var {
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

#[derive(Debug)]
pub enum Val<'a> {
    Ctor(Ctor, Vec<Rc<RefCell<RcVal<'a>>>>),
    Int(i32),
    PAp(&'a Def<Expr>, Vec<Rc<RefCell<RcVal<'a>>>>),
}

// A reference-counted value.
// This is currently just a simulation, since the interpeter passes values around using Rc, which
// does its own reference counting.
#[derive(Debug)]
pub enum RcVal<'a> {
    Val { rc: AtomicUsize, inner: Val<'a> },
    Null,
}

impl<'a> RcVal<'a> {
    pub fn new(inner: Val) -> RcVal {
        RcVal::Val {
            rc: AtomicUsize::new(1),
            inner,
        }
    }
    pub fn inc(&mut self) {
        match self {
            RcVal::Null => {
                panic!("Cannot increment reference count of NULL");
            }
            RcVal::Val { rc, inner } => {
                use std::sync::atomic::Ordering;
                let prev = rc.fetch_add(1, Ordering::SeqCst);
                println!("rc {} -> {}: {:?}", prev, prev + 1, inner);
            }
        }
    }
    pub fn dec(&mut self) {
        match self {
            RcVal::Null => {
                panic!("Cannot decrement reference count of NULL");
            }
            RcVal::Val { rc, inner } => {
                use std::sync::atomic::Ordering;
                let prev = rc.fetch_sub(1, Ordering::SeqCst);
                println!("rc {} -> {}: {:?}", prev, prev - 1, inner);
                if prev == 1 {
                    println!("DROP {:?}", inner);
                    // dec children
                    match &inner {
                        Val::Ctor(_, args) => {
                            for arg in args {
                                arg.borrow_mut().dec();
                            }
                        }
                        Val::PAp(_, args) => {
                            for arg in args {
                                arg.borrow_mut().dec();
                            }
                        }
                        _ => {}
                    };
                    // TODO: drop self (how?)
                    *self = RcVal::Null;
                }
            }
        }
    }
    pub fn inner(&self) -> &Val<'a> {
        match self {
            RcVal::Null => panic!("Cannot get inner of NULL"),
            RcVal::Val { inner, .. } => inner,
        }
    }
}

// A value that contains no partial applications
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DataVal {
    Ctor(Ctor, Vec<DataVal>),
    Int(i32),
}

/// Convert a `Val` to a `DataVal`.
/// This performs a deep copy of the value, and makes it easier to extract a result from
/// evaluation.
fn to_data_val<'a>(val: &Rc<RefCell<RcVal<'a>>>) -> DataVal {
    match val.borrow().inner() {
        Val::Ctor(ctor, xs) => {
            DataVal::Ctor(ctor.clone(), xs.iter().map(|x| to_data_val(x)).collect())
        }
        Val::Int(n) => DataVal::Int(*n),
        Val::PAp(_, _) => {
            panic!("Cannot convert partial application to DataVal");
        }
    }
}

// Evaluation
// We evaluate nameless expressions
// We have two stacks: function args and local variables.
// Each stack element is a reference to a heap-allocated value: `Box<Val>`.
// Values are normal forms: ctors, ints, paps
// Evaluation returns a value
// We store all values in Rc cells

// Evaluation context
// This tells us where we are in the parent expression, so know what to do when we've finished
// evaluating some subexpression.
#[derive(Debug)]
pub enum Ctx<'a> {
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
        func: Rc<RefCell<RcVal<'a>>>,
        arg_vals: Vec<Rc<RefCell<RcVal<'a>>>>,
    },
    ArgDrop(usize),
    EvalVars0 {
        vars: Vec<Var>,
        callback: EvalVarsCallback<'a>,
    },
    EvalVarsFromResult {
        vars: Vec<Var>,
        vals: Vec<Rc<RefCell<RcVal<'a>>>>,
        callback: EvalVarsCallback<'a>,
    },
    EvalVars {
        vars: Vec<Var>,
        vals: Vec<Rc<RefCell<RcVal<'a>>>>,
        callback: EvalVarsCallback<'a>,
    },
    CtorFinish {
        ctor: Ctor,
        arg_vals: Vec<Rc<RefCell<RcVal<'a>>>>,
    },
    Case {
        alts: &'a [(Ctor, Expr)],
    },
    Prim {
        prim: Prim,
        arg_vals: Vec<Rc<RefCell<RcVal<'a>>>>,
    },
    IncVar,
    DecVar,
}

pub fn eval<'a>(
    env: &'a HashMap<String, Def<Expr>>,
    mut args: Stack<Rc<RefCell<RcVal<'a>>>>,
    mut locals: Stack<Rc<RefCell<RcVal<'a>>>>,
    start_expr: &'a Expr,
) -> DataVal {
    // Used to store the result of an evaluation
    // We initially set it to False/Nil/Nothing
    let mut result = Rc::new(RefCell::new(RcVal::new(Val::Ctor(Ctor { tag: 0 }, vec![]))));
    // Used to store the parent context, when we have to evaluate sub-expressions.
    let mut ctx: Vec<Ctx> = vec![Ctx::Eval(start_expr)];

    loop {
        match ctx.pop() {
            None => {
                return to_data_val(&result);
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
                match func.borrow().inner() {
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
                            result =
                                Rc::new(RefCell::new(RcVal::new(Val::PAp(def, existing_args))));
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
                result = Rc::new(RefCell::new(RcVal::new(Val::Ctor(ctor, arg_vals))));
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
                match target.borrow().inner() {
                    Val::Ctor(Ctor { tag }, ctor_args) => {
                        // Case alts are ordered by tag, so we jump directly to the right one
                        let (_, rhs) = &alts[*tag];
                        // Drop ctor arg bindings after eval
                        for _ in ctor_args {
                            ctx.push(Ctx::LetDrop);
                        }
                        // Push ctor_args onto locals stack
                        locals.push_chunk(ctor_args.clone());
                        // Eval rhs
                        ctx.push(Ctx::Eval(rhs))
                    }
                    _ => panic!("Case analysis on non-constructor {:?}", target),
                };
            }
            Some(Ctx::Prim { prim, arg_vals }) => {
                result = match (&prim, &arg_vals[..]) {
                    (Prim::IntAdd, [xref, yref]) => {
                        match (xref.borrow().inner(), yref.borrow().inner()) {
                            (&Val::Int(x), &Val::Int(y)) => {
                                Rc::new(RefCell::new(RcVal::new(Val::Int(x + y))))
                            }
                            _ => panic!("prim + applied to bad args: {:?}", arg_vals),
                        }
                    }
                    (Prim::IntSub, [xref, yref]) => {
                        match (xref.borrow().inner(), yref.borrow().inner()) {
                            (&Val::Int(x), &Val::Int(y)) => {
                                Rc::new(RefCell::new(RcVal::new(Val::Int(x - y))))
                            }
                            _ => panic!("prim - applied to bad args: {:?}", arg_vals),
                        }
                    }
                    (Prim::IntEq, [xref, yref]) => {
                        match (xref.borrow().inner(), yref.borrow().inner()) {
                            (&Val::Int(x), &Val::Int(y)) => {
                                Rc::new(RefCell::new(RcVal::new(Val::Ctor(
                                    Ctor {
                                        tag: (x == y) as usize,
                                    },
                                    vec![],
                                ))))
                            }
                            _ => panic!("prim ==(int) applied to bad args: {:?}", arg_vals),
                        }
                    }
                    (Prim::IntLt, [xref, yref]) => {
                        match (xref.borrow().inner(), yref.borrow().inner()) {
                            (&Val::Int(x), &Val::Int(y)) => {
                                Rc::new(RefCell::new(RcVal::new(Val::Ctor(
                                    Ctor {
                                        tag: (x < y) as usize,
                                    },
                                    vec![],
                                ))))
                            }
                            _ => panic!("prim ==(int) applied to bad args: {:?}", arg_vals),
                        }
                    }
                    (Prim::Panic, _) => panic!("Panic"),
                    _ => panic!("prim {:?} applied to bad args: {:?}", prim, arg_vals),
                };
            }
            Some(Ctx::IncVar) => {
                result.as_ref().borrow_mut().inc();
            }
            Some(Ctx::DecVar) => {
                result.as_ref().borrow_mut().dec();
            }
            Some(Ctx::Eval(expr)) => {
                match expr {
                    Expr::Int(n) => {
                        result = Rc::new(RefCell::new(RcVal::new(Val::Int(*n))));
                    }
                    Expr::Var(v) => {
                        ctx.push(Ctx::EvalVar(v.clone()));
                    }
                    Expr::App(f, xs) => match lookup_var(env, &args, &locals, f) {
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
                            result = Rc::new(RefCell::new(RcVal::new(Val::Ctor(*ctor, vec![]))));
                        } else {
                            ctx.push(Ctx::EvalVars0 {
                                vars: vars.clone(),
                                callback: EvalVarsCallback::MakeCtorFinish { ctor: *ctor },
                            });
                        }
                    }
                    Expr::Case(target, alts) => {
                        ctx.push(Ctx::Case { alts });
                        ctx.push(Ctx::EvalVar(target.clone()));
                    }
                    Expr::Prim(prim, vars) => {
                        ctx.push(Ctx::EvalVars0 {
                            vars: vars.clone(),
                            callback: EvalVarsCallback::MakePrim { prim: *prim },
                        });
                    }
                    // inc and dec are currently no-ops
                    Expr::Inc(v, e) => {
                        ctx.push(Ctx::Eval(e));
                        ctx.push(Ctx::IncVar);
                        ctx.push(Ctx::EvalVar(v.clone()));
                    }
                    Expr::Dec(v, e) => {
                        ctx.push(Ctx::Eval(e));
                        ctx.push(Ctx::DecVar);
                        ctx.push(Ctx::EvalVar(v.clone()));
                    }
                }
            }
        }
    }
}

// A callback for EvalVars
// This should take the values and produce a new Ctx
#[derive(Debug)]
pub enum EvalVarsCallback<'a> {
    MakeAppCall { func: Rc<RefCell<RcVal<'a>>> },
    MakeCtorFinish { ctor: Ctor },
    MakePrim { prim: Prim },
}

impl<'a> EvalVarsCallback<'a> {
    fn call(self, arg_vals: Vec<Rc<RefCell<RcVal<'a>>>>) -> Ctx<'a> {
        match self {
            EvalVarsCallback::MakeAppCall { func } => Ctx::AppCall { func, arg_vals },
            EvalVarsCallback::MakeCtorFinish { ctor } => Ctx::CtorFinish { ctor, arg_vals },
            EvalVarsCallback::MakePrim { prim } => Ctx::Prim { prim, arg_vals },
        }
    }
}

#[derive(Debug)]
pub enum VarLookup<'a> {
    NullaryDef(&'a Def<Expr>),
    Val(Rc<RefCell<RcVal<'a>>>),
}

fn lookup_var<'a, 'b>(
    env: &'a HashMap<String, Def<Expr>>,
    args: &'b Stack<Rc<RefCell<RcVal<'a>>>>,
    locals: &'b Stack<Rc<RefCell<RcVal<'a>>>>,
    var: &'b Var,
) -> VarLookup<'a> {
    match var {
        Var::Global(name) => match env.get(name) {
            Some(def) if def.arity == 0 => VarLookup::NullaryDef(def),
            Some(def) => VarLookup::Val(Rc::new(RefCell::new(RcVal::new(Val::PAp(def, vec![]))))),
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
pub struct Stack<T> {
    inner: Vec<T>,
}

impl<T> Stack<T> {
    pub fn new() -> Self {
        Stack { inner: vec![] }
    }
    pub fn push(&mut self, x: T) {
        self.inner.push(x);
    }
    pub fn push_chunk(&mut self, mut xs: Vec<T>) {
        self.inner.append(&mut xs);
    }
    pub fn pop(&mut self) {
        self.inner.pop();
    }
    pub fn drop(&mut self, n: usize) {
        self.inner.truncate(self.inner.len() - n);
    }
}

impl<T> Default for Stack<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> std::ops::Index<usize> for Stack<T> {
    type Output = T;
    fn index(&self, index: usize) -> &T {
        let i = self.inner.len() - 1 - index;
        &self.inner[i]
    }
}

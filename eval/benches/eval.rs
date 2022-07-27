use std::collections::HashMap;

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};

use eval::dsl::*;
use eval::eval::{eval, make_nameless_env, Ctor, DataVal, Def, Expr, Prim, Stack};

mod inst;

fn eval_main<'a>(env: &'a HashMap<String, Def<Expr>>) -> DataVal {
    eval(
        &env,
        Stack::new(),
        Stack::new(),
        &env.get("main").unwrap().expr,
    )
}

fn fib_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("fib");
    for (size, expected_result) in [(2, 1), (4, 3), (8, 21), (16, 987), (32, 2_178_309)].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            b.iter(|| {
                let result = eval_main(&make_fib_program(size));
                assert_eq!(result, DataVal::Int(*expected_result));
            })
        });
    }
    group.finish();
}

fn last_benchmark(c: &mut Criterion) {
    c.bench_function("last", |b| {
        b.iter(|| {
            let result = eval_main(black_box(&make_last_program()));
            assert_eq!(
                result,
                DataVal::Ctor(Ctor { tag: 1 }, vec![DataVal::Int(2)])
            );
        })
    });
}

criterion_group!(benches, fib_benchmark, inst::fib_benchmark, last_benchmark);
criterion_main!(benches);

fn make_fib_program(n: i32) -> HashMap<String, Def<Expr>> {
    let fib = Def {
        name: "fib".into(),
        arity: 1,
        params: vec!["n".into()],
        expr: let_(
            "zero",
            int(0),
            let_(
                "one",
                int(1),
                let_(
                    "two",
                    int(2),
                    let_(
                        "neq0",
                        prim(Prim::IntEq, vec![arg("n"), local("zero")]),
                        case(
                            local("neq0"),
                            vec![
                                (
                                    ctor_pat("False", 0, vec![]),
                                    let_(
                                        "neq1",
                                        prim(Prim::IntEq, vec![arg("n"), local("one")]),
                                        case(
                                            local("neq1"),
                                            vec![
                                                (
                                                    ctor_pat("False", 0, vec![]),
                                                    let_(
                                                        "nil",
                                                        ctor_("Nil", 0, vec![]),
                                                        let_(
                                                            "fib_build_args0",
                                                            ctor_(
                                                                "Cons",
                                                                1,
                                                                vec![local("one"), local("nil")],
                                                            ),
                                                            let_(
                                                                "fib_build_args",
                                                                ctor_(
                                                                    "Cons",
                                                                    1,
                                                                    vec![
                                                                        local("one"),
                                                                        local("fib_build_args0"),
                                                                    ],
                                                                ),
                                                                let_(
                                                                    "fibs",
                                                                    app(
                                                                        global("fib_build"),
                                                                        vec![
                                                                            arg("n"),
                                                                            local("fib_build_args"),
                                                                        ],
                                                                    ),
                                                                    case(
                                                                        local("fibs"),
                                                                        vec![
                                                                            (
                                                                                ctor_pat(
                                                                                    "Nil",
                                                                                    0,
                                                                                    vec![],
                                                                                ),
                                                                                prim(
                                                                                    Prim::Panic,
                                                                                    vec![],
                                                                                ),
                                                                            ),
                                                                            (
                                                                                ctor_pat(
                                                                                    "Cons",
                                                                                    1,
                                                                                    vec!["x", "xs"],
                                                                                ),
                                                                                var(local("x")),
                                                                            ),
                                                                        ],
                                                                    ),
                                                                ),
                                                            ),
                                                        ),
                                                    ),
                                                ),
                                                (ctor_pat("True", 1, vec![]), var(local("one"))),
                                            ],
                                        ),
                                    ),
                                ),
                                (ctor_pat("True", 1, vec![]), var(local("one"))),
                            ],
                        ),
                    ),
                ),
            ),
        ),
    };
    let fib_build = def(
        "fib_build",
        vec!["n", "ms"],
        let_(
            "ms_len",
            app(global("length"), vec![arg("ms")]),
            let_(
                "n<=ms_len",
                app(global("<="), vec![arg("n"), local("ms_len")]),
                case(
                    local("n<=ms_len"),
                    vec![
                        (
                            ctor_pat("False", 0, vec![]),
                            let_(
                                "ms'",
                                app(global("fib'"), vec![arg("ms")]),
                                app(global("fib_build"), vec![arg("n"), local("ms'")]),
                            ),
                        ),
                        (ctor_pat("True", 1, vec![]), var(arg("ms"))),
                    ],
                ),
            ),
        ),
    );

    let fib_prime = def(
        "fib'",
        vec!["ms"],
        case(
            arg("ms"),
            vec![
                (ctor_pat("Nil", 0, vec![]), prim(Prim::Panic, vec![])),
                (
                    ctor_pat("Cons", 1, vec!["x", "ms'"]),
                    case(
                        local("ms'"),
                        vec![
                            (ctor_pat("Nil", 0, vec![]), prim(Prim::Panic, vec![])),
                            (
                                ctor_pat("Cons", 1, vec!["y", "ms''"]),
                                let_(
                                    "r",
                                    prim(Prim::IntAdd, vec![local("x"), local("y")]),
                                    let_(
                                        "l0",
                                        ctor_("Cons", 1, vec![local("y"), local("ms''")]),
                                        let_(
                                            "l1",
                                            ctor_("Cons", 1, vec![local("x"), local("l0")]),
                                            let_(
                                                "l2",
                                                ctor_("Cons", 1, vec![local("r"), local("l1")]),
                                                var(local("l2")),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ],
                    ),
                ),
            ],
        ),
    );
    let length = def(
        "length",
        vec!["l"],
        case(
            arg("l"),
            vec![
                (ctor_pat("Nil", 0, vec![]), int(0)),
                (
                    ctor_pat("Cons", 1, vec!["x", "xs"]),
                    let_(
                        "n",
                        app(global("length"), vec![local("xs")]),
                        let_(
                            "1",
                            int(1),
                            prim(Prim::IntAdd, vec![local("n"), local("1")]),
                        ),
                    ),
                ),
            ],
        ),
    );
    let lt = def(
        "<=",
        vec!["x", "y"],
        let_(
            "x<y",
            prim(Prim::IntLt, vec![arg("x"), arg("y")]),
            case(
                local("x<y"),
                vec![
                    (
                        ctor_pat("False", 0, vec![]),
                        prim(Prim::IntEq, vec![arg("x"), arg("y")]),
                    ),
                    (ctor_pat("True", 1, vec![]), var(local("x<y"))),
                ],
            ),
        ),
    );
    let main = def(
        "main",
        vec![],
        let_("n", int(n), app(global("fib"), vec![local("n")])),
    );
    make_nameless_env(vec![main, lt, length, fib, fib_prime, fib_build])
}

fn make_last_program() -> HashMap<String, Def<Expr>> {
    let last = def(
        "last",
        vec!["l"],
        case(
            arg("l"),
            vec![
                (
                    ctor_pat("Nil", 0, vec![]),
                    dec(arg("l"), ctor_("Nothing", 0, vec![])),
                ),
                (
                    ctor_pat("Cons", 1, vec!["x", "xs"]),
                    inc(
                        local("x"),
                        inc(
                            local("xs"),
                            dec(
                                arg("l"),
                                case(
                                    local("xs"),
                                    vec![
                                        (
                                            ctor_pat("Nil", 0, vec![]),
                                            dec(local("xs"), ctor_("Just", 1, vec![local("x")])),
                                        ),
                                        (
                                            ctor_pat("Cons", 1, vec!["y", "ys"]),
                                            dec(local("x"), app(global("last"), vec![local("xs")])),
                                        ),
                                    ],
                                ),
                            ),
                        ),
                    ),
                ),
            ],
        ),
    );
    // main = let one = 1
    //            two = 2
    //            nil = Nil
    //            l1 = Cons two nil
    //            l2 = Cons one l1
    //         in last l2
    let main = def(
        "main",
        vec![],
        let_(
            "one",
            int(1),
            let_(
                "two",
                int(2),
                let_(
                    "nil",
                    ctor_("Nil", 0, vec![]),
                    let_(
                        "l1",
                        ctor_("Cons", 1, vec![local("two"), local("nil")]),
                        let_(
                            "l2",
                            ctor_("Cons", 1, vec![local("one"), local("l1")]),
                            app(global("last"), vec![local("l2")]),
                        ),
                    ),
                ),
            ),
        ),
    );
    make_nameless_env(vec![main, last])
}

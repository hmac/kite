#[cfg(test)]
use pretty_assertions::assert_eq;

use crate::dsl::*;
use crate::eval::*;

#[cfg(test)]
fn assert_eval<'a>(defs: Vec<Def<NamedExpr>>, expected: DataVal) {
    let env = make_nameless_env(defs);
    assert_eq!(
        eval(
            &env,
            Stack::new(),
            Stack::new(),
            &env.get("main").unwrap().expr,
        ),
        expected
    );
}
#[test]
fn test_1() {
    let unit = ctor_("Unit", 0, vec![]);
    let nil = ctor_("Nil", 0, vec![]);

    // let nil = Nil
    //  in let unit = Unit
    //      in Cons unit nil
    let main = Def {
        name: "main".into(),
        arity: 0,
        params: vec![],
        expr: let_(
            "nil",
            nil,
            let_(
                "unit",
                unit,
                ctor_("Cons", 1, vec![local("unit"), local("nil")]),
            ),
        ),
    };
    assert_eval(
        vec![main],
        val_ctor(1, vec![val_ctor(0, vec![]), val_ctor(0, vec![])]),
    );
}

#[test]
fn test_2() {
    let id = def("id", vec!["x"], var(arg("x")));
    let unit = def("unit", vec![], ctor_("Unit", 0, vec![]));
    let main = def("main", vec![], app(global("id"), vec![global("unit")]));

    let env = make_nameless_env(vec![id.clone(), unit.clone(), main.clone()]);

    assert_eq!(env.get("id").unwrap().expr, Expr::Var(Var::Arg(0)));
    assert_eq!(
        env.get("unit").unwrap().expr,
        Expr::Ctor(Ctor { tag: 0 }, vec![])
    );
    assert_eq!(
        env.get("main").unwrap().expr,
        Expr::App(Var::Global("id".into()), vec![Var::Global("unit".into())])
    );

    assert_eval(vec![id, unit, main], val_ctor(0, vec![]));
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
                    Pat::Ctor(false_ctor.clone(), vec![]),
                    NamedExpr::Ctor(true_ctor.clone(), vec![]),
                ),
                (
                    Pat::Ctor(true_ctor.clone(), vec![]),
                    NamedExpr::Ctor(false_ctor.clone(), vec![]),
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

    let env = make_nameless_env(vec![not.clone(), main.clone()]);

    assert_eq!(
        env.get("not").unwrap().expr,
        Expr::Case(
            Var::Arg(0),
            vec![
                (Ctor { tag: 0 }, Expr::Ctor(Ctor { tag: 1 }, vec![])),
                (Ctor { tag: 1 }, Expr::Ctor(Ctor { tag: 0 }, vec![])),
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

    assert_eval(vec![not, main], val_ctor(1, vec![]));
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
                    Pat::Ctor(nothing_ctor.clone(), vec![]),
                    NamedExpr::Var(NamedVar::Arg("d".into())),
                ),
                (
                    Pat::Ctor(just_ctor.clone(), vec!["x".into()]),
                    NamedExpr::Var(NamedVar::Local("x".into())),
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

    let env = make_nameless_env(vec![from_maybe.clone(), main.clone()]);

    assert_eq!(
        env.get("fromMaybe").unwrap().expr,
        Expr::Case(
            Var::Arg(0),
            vec![
                (Ctor { tag: 0 }, Expr::Var(Var::Arg(1)),),
                (Ctor { tag: 1 }, Expr::Var(Var::Local(0)),),
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

    assert_eval(vec![from_maybe, main], val_int(2));
}

#[cfg(test)]
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
                                vec![NamedVar::Local("x'".into()), NamedVar::Local("xs'".into())],
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
                    Pat::Ctor(false_ctor.clone(), vec![]),
                    NamedExpr::Ctor(true_ctor.clone(), vec![]),
                ),
                (
                    Pat::Ctor(true_ctor.clone(), vec![]),
                    NamedExpr::Ctor(false_ctor.clone(), vec![]),
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
                                vec![NamedVar::Local("true".into()), NamedVar::Local("l1".into())],
                            )),
                            Box::new(NamedExpr::App(
                                NamedVar::Global("map".into()),
                                vec![NamedVar::Global("not".into()), NamedVar::Local("l2".into())],
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

    assert_eval(
        vec![not, map_example(), main],
        val_ctor(
            1,
            vec![
                val_ctor(0, vec![]),
                val_ctor(1, vec![val_ctor(1, vec![]), val_ctor(0, vec![])]),
            ],
        ),
    );
}

#[test]
fn test_6() {
    let pair_ctor = NamedCtor {
        name: "Pair".into(),
        tag: 0,
    };
    let true_ctor = NamedCtor {
        name: "True".into(),
        tag: 1,
    };
    let false_ctor = NamedCtor {
        name: "False".into(),
        tag: 0,
    };
    let swap = Def {
        name: "swap".into(),
        arity: 1,
        params: vec!["p".into()],
        expr: NamedExpr::Case(
            NamedVar::Arg("p".into()),
            vec![(
                Pat::Ctor(pair_ctor.clone(), vec!["x".into(), "y".into()]),
                NamedExpr::Ctor(
                    pair_ctor.clone(),
                    vec![NamedVar::Local("y".into()), NamedVar::Local("x".into())],
                ),
            )],
        ),
    };
    let pair = Def {
        name: "pair".into(),
        arity: 2,
        params: vec!["x".into(), "y".into()],
        expr: NamedExpr::Ctor(
            pair_ctor.clone(),
            vec![NamedVar::Arg("x".into()), NamedVar::Arg("y".into())],
        ),
    };
    let main = Def {
        name: "main".into(),
        arity: 0,
        params: vec![],
        expr: NamedExpr::Let(
            "true".into(),
            Box::new(NamedExpr::Ctor(true_ctor.clone(), vec![])),
            Box::new(NamedExpr::Let(
                "false".into(),
                Box::new(NamedExpr::Ctor(false_ctor.clone(), vec![])),
                Box::new(NamedExpr::Let(
                    "p".into(),
                    Box::new(NamedExpr::App(
                        NamedVar::Global("pair".into()),
                        vec![
                            NamedVar::Local("true".into()),
                            NamedVar::Local("false".into()),
                        ],
                    )),
                    Box::new(NamedExpr::App(
                        NamedVar::Global("swap".into()),
                        vec![NamedVar::Local("p".into())],
                    )),
                )),
            )),
        ),
    };

    assert_eval(
        vec![swap, pair, main],
        val_ctor(0, vec![val_ctor(0, vec![]), val_ctor(1, vec![])]),
    );
}

#[test]
fn test_7() {
    let nil_ctor = NamedCtor {
        name: "Nil".into(),
        tag: 0,
    };
    let cons_ctor = NamedCtor {
        name: "Cons".into(),
        tag: 1,
    };
    let list = Def {
        name: "list".into(),
        arity: 0,
        params: vec![],
        expr: NamedExpr::Let(
            "nil".into(),
            Box::new(NamedExpr::Ctor(nil_ctor.clone(), vec![])),
            Box::new(NamedExpr::Let(
                "x0".into(),
                Box::new(NamedExpr::Int(0)),
                Box::new(NamedExpr::Let(
                    "x1".into(),
                    Box::new(NamedExpr::Int(1)),
                    Box::new(NamedExpr::Let(
                        "x2".into(),
                        Box::new(NamedExpr::Int(2)),
                        Box::new(NamedExpr::Let(
                            "x3".into(),
                            Box::new(NamedExpr::Int(3)),
                            Box::new(NamedExpr::Let(
                                "l1".into(),
                                Box::new(NamedExpr::Ctor(
                                    cons_ctor.clone(),
                                    vec![
                                        NamedVar::Local("x3".into()),
                                        NamedVar::Local("nil".into()),
                                    ],
                                )),
                                Box::new(NamedExpr::Let(
                                    "l2".into(),
                                    Box::new(NamedExpr::Ctor(
                                        cons_ctor.clone(),
                                        vec![
                                            NamedVar::Local("x2".into()),
                                            NamedVar::Local("l1".into()),
                                        ],
                                    )),
                                    Box::new(NamedExpr::Let(
                                        "l3".into(),
                                        Box::new(NamedExpr::Ctor(
                                            cons_ctor.clone(),
                                            vec![
                                                NamedVar::Local("x1".into()),
                                                NamedVar::Local("l2".into()),
                                            ],
                                        )),
                                        Box::new(NamedExpr::Let(
                                            "l4".into(),
                                            Box::new(NamedExpr::Ctor(
                                                cons_ctor.clone(),
                                                vec![
                                                    NamedVar::Local("x0".into()),
                                                    NamedVar::Local("l3".into()),
                                                ],
                                            )),
                                            Box::new(NamedExpr::Var(NamedVar::Local("l4".into()))),
                                        )),
                                    )),
                                )),
                            )),
                        )),
                    )),
                )),
            )),
        ),
    };
    let inc = Def {
        name: "inc".into(),
        arity: 1,
        params: vec!["x".into()],
        expr: NamedExpr::Let(
            "one".into(),
            Box::new(NamedExpr::Int(1)),
            Box::new(NamedExpr::Prim(
                Prim::IntAdd,
                vec![NamedVar::Arg("x".into()), NamedVar::Local("one".into())],
            )),
        ),
    };
    let main = Def {
        name: "main".into(),
        arity: 0,
        params: vec![],
        expr: NamedExpr::App(
            NamedVar::Global("map".into()),
            vec![
                NamedVar::Global("inc".into()),
                NamedVar::Global("list".into()),
            ],
        ),
    };
    assert_eval(
        vec![inc, list, map_example(), main],
        val_ctor(
            1,
            vec![
                val_int(1),
                val_ctor(
                    1,
                    vec![
                        val_int(2),
                        val_ctor(
                            1,
                            vec![
                                val_int(3),
                                val_ctor(1, vec![val_int(4), val_ctor(0, vec![])]),
                            ],
                        ),
                    ],
                ),
            ],
        ),
    );
}

// sum_to = n -> case n == 0 of
//                 True -> 0
//                 False -> n + (sum_to (n - 1))
// main = let n = 5 in sum_to n
#[test]
fn test_8() {
    let sum_to = Def {
        name: "sum_to".into(),
        arity: 1,
        params: vec!["n".into()],
        expr: NamedExpr::Let(
            "zero".into(),
            Box::new(NamedExpr::Int(0)),
            Box::new(NamedExpr::Let(
                "one".into(),
                Box::new(NamedExpr::Int(1)),
                Box::new(NamedExpr::Let(
                    "neq0".into(),
                    Box::new(NamedExpr::Prim(
                        Prim::IntEq,
                        vec![NamedVar::Arg("n".into()), NamedVar::Local("zero".into())],
                    )),
                    Box::new(NamedExpr::Case(
                        NamedVar::Local("neq0".into()),
                        vec![
                            (
                                Pat::Ctor(
                                    NamedCtor {
                                        name: "False".into(),
                                        tag: 0,
                                    },
                                    vec![],
                                ),
                                NamedExpr::Let(
                                    "n-1".into(),
                                    Box::new(NamedExpr::Prim(
                                        Prim::IntSub,
                                        vec![
                                            NamedVar::Arg("n".into()),
                                            NamedVar::Local("one".into()),
                                        ],
                                    )),
                                    Box::new(NamedExpr::Let(
                                        "sum".into(),
                                        Box::new(NamedExpr::App(
                                            NamedVar::Global("sum_to".into()),
                                            vec![NamedVar::Local("n-1".into())],
                                        )),
                                        Box::new(NamedExpr::Prim(
                                            Prim::IntAdd,
                                            vec![
                                                NamedVar::Local("sum".into()),
                                                NamedVar::Arg("n".into()),
                                            ],
                                        )),
                                    )),
                                ),
                            ),
                            (
                                Pat::Ctor(
                                    NamedCtor {
                                        name: "True".into(),
                                        tag: 1,
                                    },
                                    vec![],
                                ),
                                NamedExpr::Var(NamedVar::Local("zero".into())),
                            ),
                        ],
                    )),
                )),
            )),
        ),
    };
    let main = Def {
        name: "main".into(),
        arity: 0,
        params: vec![],
        expr: NamedExpr::Let(
            "n".into(),
            Box::new(NamedExpr::Int(5)),
            Box::new(NamedExpr::App(
                NamedVar::Global("sum_to".into()),
                vec![NamedVar::Local("n".into())],
            )),
        ),
    };
    assert_eval(vec![sum_to, main], val_int(15));
}

// fib 0 = 1
// fib 1 = 1
// fib n = case fibBuild n [1, 1] of
//   x : _xs -> x
//
// fibBuild n ms | n <= length ms = ms
// fibBuild n ms | otherwise      = fibBuild n (fib' ms)
//
// fib' ms = case ms of
//   x : ms' -> case ms' of
//     y : ms'' -> let r = x + y in r : x : y : ms''
#[test]
fn test_9() {
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
        let_("n", int(15), app(global("fib"), vec![local("n")])),
    );
    assert_eval(
        vec![main, lt, length, fib, fib_prime, fib_build],
        val_int(610),
    );
}

// last(l) = case l of
//   Nil -> dec l; Nothing
//   Cons x xs -> inc x; inc xs; dec l;
//     case xs of
//       Nil -> dec xs; Just x
//       Cons y ys -> dec x; last xs
#[test]
fn test_10() {
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
    assert_eval(vec![main, last], val_ctor(1, vec![val_int(2)]));
}

// fn map(f, l) = case l of
//  Nil -> dec(f); dec(l); Nil
//  Cons x xs -> inc(x)
//               inc(xs)
//               let x'  = inc(f); f x
//                   xs' = map f xs
//                in Cons x' xs'
#[test]
fn test_11() {
    let map = def(
        "map",
        vec!["f".into(), "l".into()],
        case(
            arg("l"),
            vec![
                (
                    ctor_pat("Nil", 0, vec![]),
                    dec(arg("f"), dec(arg("l"), ctor_("Nil", 0, vec![]))),
                ),
                (
                    ctor_pat("Cons", 1, vec!["x", "xs"]),
                    inc(
                        local("x"),
                        inc(
                            local("xs"),
                            let_(
                                "x'",
                                inc(arg("f"), app(arg("f"), vec![local("x")])),
                                let_(
                                    "xs'",
                                    app(global("map"), vec![arg("f"), local("xs")]),
                                    ctor_("Cons", 1, vec![local("x'"), local("xs'")]),
                                ),
                            ),
                        ),
                    ),
                ),
            ],
        ),
    );
    let list = def(
        "list",
        vec![],
        let_(
            "one",
            int(1),
            let_(
                "two",
                int(2),
                let_(
                    "three",
                    int(3),
                    let_(
                        "nil",
                        ctor_("Nil", 0, vec![]),
                        let_(
                            "l1",
                            ctor_("Cons", 1, vec![local("three"), local("nil")]),
                            let_(
                                "l2",
                                ctor_("Cons", 1, vec![local("two"), local("l1")]),
                                let_(
                                    "l3",
                                    ctor_("Cons", 1, vec![local("one"), local("l2")]),
                                    var(local("l3")),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    );
    // fn inc(x) = let one = 1
    //                 r   = x + one
    //              in dec(one); r
    let inc = def(
        "inc",
        vec!["x"],
        let_(
            "one",
            int(1),
            let_(
                "r",
                prim(Prim::IntAdd, vec![arg("x"), local("one")]),
                dec(local("one"), var(local("r"))),
            ),
        ),
    );
    let main = def(
        "main",
        vec![],
        app(global("map"), vec![global("inc"), global("list")]),
    );
    assert_eval(
        vec![main, map, list, inc],
        val_ctor(
            1,
            vec![
                val_int(2),
                val_ctor(
                    1,
                    vec![
                        val_int(3),
                        val_ctor(1, vec![val_int(4), val_ctor(0, vec![])]),
                    ],
                ),
            ],
        ),
    );
}

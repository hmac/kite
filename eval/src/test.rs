use super::*;
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

use std::collections::HashMap;

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};

use eval::inst::{eval, DataValue, Inst, IntArg, StackAddr, StackValue};

pub fn fib_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("fib_inst");
    for (size, expected_result) in [(2, 1), (4, 3), (8, 21), (16, 987), (32, 2_178_309)].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            b.iter(|| {
                let result = eval(&make_fib_program(size)).to_data_value();
                assert_eq!(result, DataValue::Int(*expected_result));
            })
        });
    }
    group.finish();
}

fn make_fib_program(n: i32) -> Vec<Inst> {
    vec![
        // call to main
        Inst::Int(0),
        Inst::Func { arity: 0, addr: 4 },
        Inst::Call,
        Inst::Halt,
        // [4] main = fib <n>
        Inst::Int(n),
        Inst::Int(1),
        Inst::Func { arity: 1, addr: 9 },
        Inst::Call,
        Inst::Ret,
        // [9] fib n = go n 1 1
        Inst::Int(1),
        Inst::Int(1),
        Inst::Var(StackAddr::Arg(0)),
        Inst::Int(3),
        Inst::Func { arity: 3, addr: 16 },
        Inst::Call,
        Inst::Ret,
        // [16] go n x y = let n<3 = n < 3
        Inst::IntLt(IntArg::Var(StackAddr::Arg(2)), IntArg::Int(3)),
        // in case n<3 of
        Inst::Case(StackAddr::Local(0), vec![18, 27]),
        // False ->
        //   let z = x + y
        Inst::IntAdd(
            IntArg::Var(StackAddr::Arg(1)),
            IntArg::Var(StackAddr::Arg(0)),
        ),
        //       m = n - 1
        Inst::IntSub(IntArg::Var(StackAddr::Arg(2)), IntArg::Int(1)),
        //   in go m z x
        Inst::Var(StackAddr::Arg(1)),
        Inst::Var(StackAddr::Local(2)),
        Inst::Var(StackAddr::Local(2)),
        Inst::Int(3),
        Inst::Func { arity: 3, addr: 16 },
        Inst::Call,
        Inst::Ret,
        // True -> x
        Inst::Var(StackAddr::Arg(1)),
        Inst::Ret,
    ]
}

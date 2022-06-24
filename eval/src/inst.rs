use std::ops::Index;
use std::rc::Rc;

/// Instructions for the Kite VM.
/// The Kite VM has a single stack which stores all variables and intermediate results.
#[derive(Debug)]
pub enum Inst {
    Var(StackAddr),

    Int(i32),
    Call(StackAddr, Vec<StackAddr>),
    // Call to a known constant function
    CallC {
        arity: usize,
        func_addr: InstAddr,
        args: Vec<StackAddr>,
    },
    // TODO: Case expressions on integer literals
    Case(StackAddr, Vec<InstAddr>),
    Ctor(u8, Vec<StackAddr>),

    // stack operations

    // pop n elements off the stack
    Pop(usize),
    // push address onto the stack
    Push(StackAddr),
    Ret,
    Halt,

    // primitive operations
    // TODO: support integer literals in these instruction arguments, so we don't have to push them
    // onto the stack to pass them.
    IntAdd(StackAddr, StackAddr),
    IntSub(StackAddr, StackAddr),
    IntMul(StackAddr, StackAddr),
    IntEq(StackAddr, StackAddr),
    IntLt(StackAddr, StackAddr),
    IntGt(StackAddr, StackAddr),
    Panic,
}

/// The stack. This holds function arguments, local variables and intermediate results.
#[derive(Debug)]
struct Stack<T> {
    inner: Vec<T>,
    frames: Vec<usize>,
}

impl<T> Stack<T> {
    fn new() -> Self {
        Stack {
            inner: Vec::new(),
            frames: Vec::new(),
        }
    }

    fn push(&mut self, v: T) {
        self.inner.push(v);
    }

    fn pop(&mut self, n: usize) -> T {
        for _ in 0..n - 1 {
            self.inner.pop();
        }
        self.inner.pop().unwrap()
    }

    /// Remove all elements up to the last stack frame, but keep the top-most element as it holds
    /// the result.
    fn pop_frame(&mut self) {
        let r = self.inner.pop().unwrap();
        let frame = self.frames.pop().unwrap();
        println!("pop_frame: {:?}", frame);
        self.inner.truncate(frame);
        self.push(r);
    }

    /// Push a new stack frame.
    /// This just saves a marker into the current top of the stack so it can be restored later.
    fn push_frame(&mut self) {
        println!("push_frame: {:?}", self.inner.len());
        self.frames.push(self.inner.len());
    }
}

/// Stack addresses count from the top of the stack (0 is the topmost element).
impl<T> Index<usize> for Stack<T> {
    type Output = T;
    fn index(&self, i: usize) -> &T {
        let len = self.inner.len() - 1;
        &self.inner[len - i]
    }
}

#[derive(Clone, Debug)]
pub enum StackValue<I> {
    Int(i32),
    Ref(Rc<HeapValue<I>>),
    Func { arity: usize, inst: I },
}

impl<I> StackValue<I> {
    fn to_data_value(&self) -> DataValue {
        match self {
            StackValue::Int(i) => DataValue::Int(*i),
            StackValue::Ref(r) => r.to_data_value(),
            StackValue::Func { .. } => panic!("Cannot convert function to data value"),
        }
    }
}

#[derive(Debug)]
pub enum HeapValue<I> {
    Ctor(u8, Vec<StackValue<I>>),
    PAp {
        inst: I,
        arity: usize,
        args: Vec<StackValue<I>>,
    },
}

impl<I> HeapValue<I> {
    fn to_data_value(&self) -> DataValue {
        match self {
            HeapValue::Ctor(tag, args) => {
                let args = args.iter().map(|a| a.to_data_value()).collect();
                DataValue::Ctor(*tag, args)
            }
            HeapValue::PAp { .. } => panic!("Cannot convert PAp to data value"),
        }
    }
}

/// A fully evaluated value.
#[derive(Debug, PartialEq)]
pub enum DataValue {
    Int(i32),
    Ctor(u8, Vec<DataValue>),
}

type InstAddr = usize;
type StackAddr = usize;

/// Evaluate a sequence of instructions.
pub fn eval(insts: &[Inst]) -> StackValue<InstAddr> {
    let mut stack: Stack<StackValue<InstAddr>> = Stack::new();
    let mut call_stack: Stack<InstAddr> = Stack::new();
    let mut inst_addr = 0;
    let mut bail_counter = 0;
    while inst_addr < insts.len() && bail_counter < 1000 {
        bail_counter += 1;
        println!("stack {:?}", stack.inner);
        println!("inst: {:?}", insts[inst_addr]);
        match eval_inst(&mut stack, &mut call_stack, inst_addr, insts) {
            Some(i) => {
                inst_addr = i;
            }
            None => {
                break;
            }
        }
    }
    return stack[0].clone();
}

/// Evaluate a single instruction.
/// If this function returns `None`, it means the VM should halt.
fn eval_inst(
    stack: &mut Stack<StackValue<InstAddr>>,
    call_stack: &mut Stack<InstAddr>,
    inst_addr: InstAddr,
    insts: &[Inst],
) -> Option<InstAddr> {
    match &insts[inst_addr] {
        Inst::Var(v) => {
            let val = stack[*v].clone();
            stack.push(val);
        }
        Inst::Int(n) => {
            stack.push(StackValue::Int(*n));
        }
        // TODO: handle case where we have too many args.
        Inst::Call(f, args) => {
            let func = &stack[*f];
            match func {
                StackValue::Int(_) => panic!("cannot call an integer"),
                StackValue::Func { inst, .. } => {
                    let func_addr = *inst;
                    // Allocate a new stack frame
                    stack.push_frame();
                    // Push args onto stack
                    // Each arg we push bumps the addresses of existing args by 1, so we have to
                    // account for that as we go.
                    for (i, arg) in args.iter().enumerate() {
                        stack.push(stack[*arg + i].clone());
                    }
                    // Push the return address onto the call stack
                    call_stack.push(inst_addr + 1);
                    // Jump to function
                    return Some(func_addr);
                }
                StackValue::Ref(ref v) => {
                    // Lookup heap value
                    // If it's not a PAp, panic
                    // Check if we have enough args to call it
                    // If yes, call it
                    match &**v {
                        HeapValue::PAp {
                            inst,
                            arity,
                            args: pap_args,
                        } => {
                            let func_addr = *inst;
                            // Clone existing_args, which copies each StackValue element
                            // This is necessary since we're going to push them onto the stack anyway
                            let existing_args = pap_args.clone();
                            let existing_args_len = existing_args.len();

                            if existing_args_len + args.len() == *arity {
                                // Allocate a new stack frame
                                stack.push_frame();

                                // Push args onto stack
                                // Each arg we push bumps the addresses of existing args by 1, so we have to
                                // account for that as we go.
                                for arg in existing_args.into_iter() {
                                    stack.push(arg);
                                }
                                for (i, arg) in args.iter().enumerate() {
                                    stack.push(stack[*arg + existing_args_len + i].clone());
                                }

                                // Push the return address onto the call stack
                                call_stack.push(inst_addr + 1);
                                // Jump to function
                                return Some(func_addr);
                            } else if existing_args_len + args.len() < *arity {
                                // allocate new PAp with old args + new args
                            } else if existing_args_len + args.len() > *arity {
                                // handle more than enough args
                                todo!("more than enough args")
                            }
                        }
                        _ => panic!("cannot call a non-function: {:?}", *v),
                    }
                }
            }
        }
        Inst::CallC {
            arity,
            func_addr,
            args,
        } => {
            if args.len() < *arity {
                // Allocate a PAp containing func_addr and args (copied from stack)
                let arg_values = args.iter().map(|a| stack[*a].clone()).collect();
                let pap = HeapValue::PAp {
                    inst: *func_addr,
                    arity: *arity,
                    args: arg_values,
                };
                // Push PAp onto stack
                stack.push(StackValue::Ref(Rc::new(pap)));
            } else {
                // TODO: handle case where we have too many args
                // Allocate a new stack frame
                stack.push_frame();
                // Push args onto stack
                // Each arg we push bumps the addresses of existing args by 1, so we have to
                // account for that as we go.
                for (i, arg) in args.iter().enumerate() {
                    stack.push(stack[*arg + i].clone());
                }
                // Push the return address onto the call stack
                call_stack.push(inst_addr + 1);
                // Jump to function
                return Some(*func_addr);
            }
        }
        Inst::Case(target, alts) => {
            let target: StackValue<InstAddr> = stack[*target].clone();
            match target {
                StackValue::Int(i) => panic!("cannot case an integer: {:?}", i),
                StackValue::Func { .. } => panic!("cannot case a function"),
                StackValue::Ref(val) => {
                    match *val {
                        HeapValue::PAp { .. } => panic!("cannot case a partial application"),
                        HeapValue::Ctor(tag, ref args) => {
                            // Push args onto stack in normal order
                            for arg in args.iter() {
                                stack.push(arg.clone());
                            }
                            // Jump to <tag>th alt
                            match alts.get(tag as usize) {
                                Some(alt) => return Some(*alt),
                                None => panic!("tag has no matching case alternative"),
                            }
                        }
                    }
                }
            }
        }
        Inst::Ctor(tag, args) => {
            let arg_vec: Vec<StackValue<InstAddr>> =
                args.iter().map(|arg| stack[*arg].clone()).collect();
            let val = HeapValue::Ctor(*tag, arg_vec);
            stack.push(StackValue::Ref(Rc::new(val)));
        }

        Inst::Pop(n) => {
            stack.pop(*n);
        }
        Inst::Push(v) => {
            stack.push(stack[*v].clone());
        }
        Inst::Ret => {
            stack.pop_frame();
            // Pop the caller's next instruction from the call stack.
            return Some(call_stack.pop(1));
        }
        Inst::Halt => {
            return None;
        }

        Inst::IntAdd(a, b) => {
            stack.push(int_binary_op(
                *a,
                *b,
                "add",
                |a, b| StackValue::Int(a + b),
                stack,
            ));
        }
        Inst::IntSub(a, b) => {
            stack.push(int_binary_op(
                *a,
                *b,
                "subtract",
                |a, b| StackValue::Int(a - b),
                stack,
            ));
        }
        Inst::IntMul(a, b) => {
            stack.push(int_binary_op(
                *a,
                *b,
                "multiply",
                |a, b| StackValue::Int(a * b),
                stack,
            ));
        }
        Inst::IntEq(a, b) => {
            stack.push(int_binary_op(
                *a,
                *b,
                "compare",
                |a, b| {
                    StackValue::Ref(Rc::new(HeapValue::Ctor(if a == b { 1 } else { 0 }, vec![])))
                },
                stack,
            ));
        }
        Inst::IntLt(a, b) => {
            stack.push(int_binary_op(
                *a,
                *b,
                "compare",
                |a, b| StackValue::Ref(Rc::new(HeapValue::Ctor(if a < b { 1 } else { 0 }, vec![]))),
                stack,
            ));
        }
        Inst::IntGt(a, b) => {
            stack.push(int_binary_op(
                *a,
                *b,
                "compare",
                |a, b| StackValue::Ref(Rc::new(HeapValue::Ctor(if a > b { 1 } else { 0 }, vec![]))),
                stack,
            ));
        }
        Inst::Panic => panic!("panic"),
    };
    Some(inst_addr + 1)
}

fn int_binary_op<I>(
    a: usize,
    b: usize,
    msg: &str,
    op: fn(i32, i32) -> StackValue<I>,
    stack: &Stack<StackValue<I>>,
) -> StackValue<I> {
    match (&stack[a], &stack[b]) {
        (StackValue::Int(a), StackValue::Int(b)) => op(*a, *b),
        _ => panic!("cannot {} non-integers", msg),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_1() {
        let insts: Vec<Inst> = vec![Inst::Int(1), Inst::Var(0)];
        assert_eq!(eval(&insts).to_data_value(), DataValue::Int(1));
    }

    #[test]
    fn test_2() {
        let insts: Vec<Inst> = vec![Inst::Int(1), Inst::Int(2), Inst::Ctor(0, vec![1, 0])];
        assert_eq!(
            eval(&insts).to_data_value(),
            DataValue::Ctor(0, vec![DataValue::Int(1), DataValue::Int(2)])
        );
    }

    // let a = 1
    //     b = 2
    //     c = Cons a b
    //  in case c of
    //       Nil -> 4
    //       Cons _ _ -> 5
    #[test]
    fn test_3() {
        let insts: Vec<Inst> = vec![
            // call to main
            Inst::CallC {
                arity: 0,
                func_addr: 2,
                args: vec![],
            },
            // exit
            Inst::Halt,
            // main
            Inst::Int(1),
            Inst::Int(2),
            Inst::Ctor(1, vec![1, 0]),
            Inst::Case(0, vec![6, 8]),
            Inst::Int(4),
            Inst::Ret,
            Inst::Int(5),
            Inst::Int(6),
            Inst::IntAdd(0, 1),
            Inst::Ret,
        ];
        assert_eq!(eval(&insts).to_data_value(), DataValue::Int(11));
    }

    // not = b -> case b of { False -> True; True -> False }
    // main = let false = False in not false
    #[test]
    fn test_4() {
        let prog: Vec<Inst> = vec![
            // call to main
            Inst::CallC {
                arity: 0,
                func_addr: 7,
                args: vec![],
            },
            Inst::Halt,
            // not = b ->
            //   case b of
            Inst::Case(0, vec![3, 5]),
            //     False -> True
            Inst::Ctor(1, vec![]),
            Inst::Ret,
            //     True -> False
            Inst::Ctor(0, vec![]),
            Inst::Ret,
            // main =
            //   let false = False in
            Inst::Ctor(0, vec![]),
            //     not false
            Inst::CallC {
                arity: 1,
                func_addr: 2,
                args: vec![0],
            },
        ];
        assert_eq!(eval(&prog).to_data_value(), DataValue::Ctor(1, vec![]));
    }

    // fromMaybe(m, d) = case m of { Just x -> x; Nothing -> d }
    // main = let one = 1 in let two = 2 in let r = Just two in fromMaybe r one
    #[test]
    fn test_5() {
        let prog: Vec<Inst> = vec![
            // call to main
            Inst::CallC {
                arity: 0,
                func_addr: 2,
                args: vec![],
            },
            Inst::Halt,
            // main =
            //   let one = 1
            Inst::Int(1),
            //       two = 2
            Inst::Int(2),
            //       r = Just two
            Inst::Ctor(1, vec![0]),
            //   in fromMaybe r one
            Inst::CallC {
                arity: 2,
                func_addr: 7,
                args: vec![0, 2],
            },
            Inst::Ret,
            // fromMaybe = m d ->
            //   case m of
            Inst::Case(1, vec![8, 10]),
            //     Just x -> x
            Inst::Var(0),
            Inst::Ret,
            //     Nothing -> d
            Inst::Var(0),
            Inst::Ret,
        ];
        assert_eq!(eval(&prog).to_data_value(), DataValue::Int(2));
    }

    // main =
    //   let nil = Nil
    //       true = True
    //       false = False
    //       l1 = Cons false nil
    //       l2 = Cons true l1
    //       not = not
    //    in map not l2
    // not = b -> case b of { True -> False; False -> True }
    // map = f l ->
    //   case l of
    //     Nil -> Nil
    //     Cons x xs -> let x' = f x
    //                      xs' = map f xs
    //                   in Cons x' xs'
    #[test]
    fn test_6() {
        let prog: Vec<Inst> = vec![
            // call to main
            Inst::CallC {
                arity: 0,
                func_addr: 2,
                args: vec![],
            },
            Inst::Halt,
            // main =
            //  let nil = Nil
            Inst::Ctor(0, vec![]),
            //      true = True
            Inst::Ctor(1, vec![]),
            //      false = False
            Inst::Ctor(0, vec![]),
            //      l1 = Cons false nil
            Inst::Ctor(1, vec![0, 2]),
            //      l2 = Cons true l1
            Inst::Ctor(1, vec![2, 0]),
            //      not = [global not]
            Inst::CallC {
                arity: 1,
                func_addr: 10,
                args: vec![],
            },
            //   in map not l2
            Inst::CallC {
                arity: 2,
                func_addr: 15,
                args: vec![0, 1],
            },
            Inst::Ret,
            // not = b ->
            //   case b of
            Inst::Case(0, vec![11, 13]),
            //     False -> True
            Inst::Ctor(1, vec![]),
            Inst::Ret,
            //     True -> False
            Inst::Ctor(0, vec![]),
            Inst::Ret,
            // map = f l ->
            //   case l of
            Inst::Case(0, vec![16, 18]),
            //     Nil -> Nil
            Inst::Ctor(0, vec![]),
            Inst::Ret,
            //     Cons x xs ->
            //       let x' = f x
            Inst::Call(3, vec![1]),
            //           xs' = map f xs
            Inst::CallC {
                arity: 2,
                func_addr: 15,
                args: vec![4, 1],
            },
            //        in Cons x' xs'
            Inst::Ctor(1, vec![1, 0]),
            Inst::Ret,
        ];
        assert_eq!(
            eval(&prog).to_data_value(),
            DataValue::Ctor(
                1,
                vec![
                    DataValue::Ctor(0, vec![]),
                    DataValue::Ctor(
                        1,
                        vec![DataValue::Ctor(1, vec![]), DataValue::Ctor(0, vec![])]
                    )
                ]
            )
        );
    }

    // main = let true = True
    //            false = False
    //            p = pair true false
    //         in swap p
    // pair = x y -> Pair x y
    // swap = p ->
    //  case p of
    //    Pair x y -> Pair y x
    #[test]
    fn test_7() {
        let prog: Vec<Inst> = vec![
            // call to main
            Inst::CallC {
                arity: 0,
                func_addr: 2,
                args: vec![],
            },
            Inst::Halt,
            // main =
            //  let true = True
            Inst::Ctor(1, vec![]),
            //      false = False
            Inst::Ctor(0, vec![]),
            //      p = pair true false
            Inst::CallC {
                arity: 2,
                func_addr: 7,
                args: vec![1, 0],
            },
            //   in swap p
            Inst::CallC {
                arity: 1,
                func_addr: 9,
                args: vec![0],
            },
            Inst::Ret,
            // pair = x y -> Pair x y
            Inst::Ctor(0, vec![1, 0]),
            Inst::Ret,
            // swap = p ->
            // case p of
            Inst::Case(0, vec![10]),
            //     Pair x y -> Pair y x
            Inst::Ctor(0, vec![0, 1]),
            Inst::Ret,
        ];
        assert_eq!(
            eval(&prog).to_data_value(),
            DataValue::Ctor(
                0,
                vec![DataValue::Ctor(0, vec![]), DataValue::Ctor(1, vec![])]
            )
        );
    }

    // main = map inc list
    // list = let nil = Nil
    //            x0 = 0
    //            x1 = 1
    //            x2 = 2
    //            x3 = 3
    //            l1 = Cons x3 nil
    //            l2 = Cons x2 l1
    //            l3 = Cons x1 l2
    //            l4 = Cons x0 l3
    //         in l4
    // inc = x -> let one = 1 in x + one
    #[test]
    fn test_8() {
        let prog: Vec<Inst> = vec![
            // call to main
            Inst::CallC {
                arity: 0,
                func_addr: 2,
                args: vec![],
            },
            Inst::Halt,
            // main = map inc list
            Inst::CallC {
                arity: 1,
                func_addr: 16,
                args: vec![],
            },
            Inst::CallC {
                arity: 0,
                func_addr: 6,
                args: vec![],
            },
            Inst::CallC {
                arity: 2,
                func_addr: 19,
                args: vec![1, 0],
            },
            Inst::Ret,
            // list =
            //   let nil = Nil
            Inst::Ctor(0, vec![]),
            //       x0 = 0
            Inst::Int(0),
            //       x1 = 1
            Inst::Int(1),
            //       x2 = 2
            Inst::Int(2),
            //       x3 = 3
            Inst::Int(3),
            //       l1 = Cons x3 nil
            Inst::Ctor(1, vec![0, 4]),
            //       l2 = Cons x2 l1
            Inst::Ctor(1, vec![2, 0]),
            //       l3 = Cons x1 l2
            Inst::Ctor(1, vec![4, 0]),
            //       l4 = Cons x0 l3
            Inst::Ctor(1, vec![6, 0]),
            //       in l4
            Inst::Ret,
            // inc = x -> let one = 1 in x + one
            Inst::Int(1),
            Inst::IntAdd(1, 0),
            Inst::Ret,
            // map = f l ->
            //   case l of
            Inst::Case(0, vec![20, 22]),
            //     Nil -> Nil
            Inst::Ctor(0, vec![]),
            Inst::Ret,
            //     Cons x xs ->
            //       let x' = f x
            Inst::Call(3, vec![1]),
            //           xs' = map f xs
            Inst::CallC {
                arity: 2,
                func_addr: 19,
                args: vec![4, 1],
            },
            //        in Cons x' xs'
            Inst::Ctor(1, vec![1, 0]),
            Inst::Ret,
        ];
        assert_eq!(
            eval(&prog).to_data_value(),
            DataValue::Ctor(
                1,
                vec![
                    DataValue::Int(1),
                    DataValue::Ctor(
                        1,
                        vec![
                            DataValue::Int(2),
                            DataValue::Ctor(
                                1,
                                vec![
                                    DataValue::Int(3),
                                    DataValue::Ctor(
                                        1,
                                        vec![DataValue::Int(4), DataValue::Ctor(0, vec![])]
                                    )
                                ]
                            )
                        ]
                    )
                ]
            )
        );
    }

    // sum_to = n ->
    //   let neq0 = n == 0
    //    in case neq0 of
    //         False -> let n-1 = n - 1
    //                      sum = sum_to n-1
    //                   in n + sum
    //         True -> 0
    // main = let n = 5
    //         in sum_to n
    #[test]
    fn test_9() {
        let prog: Vec<Inst> = vec![
            // call to main
            Inst::CallC {
                arity: 0,
                func_addr: 2,
                args: vec![],
            },
            Inst::Halt,
            // main = let n = 5
            Inst::Int(5),
            // in sum_to_n
            Inst::CallC {
                arity: 1,
                func_addr: 5,
                args: vec![0],
            },
            Inst::Ret,
            // sum_to = n -> let neq0 = n == 0
            Inst::Int(0),
            Inst::IntEq(1, 0),
            // in case neq0 of
            Inst::Case(0, vec![8, 13]),
            //  False -> let n-1 = n - 1
            Inst::Int(1),
            Inst::IntSub(3, 0),
            //  sum = sum_to n-1
            Inst::CallC {
                arity: 1,
                func_addr: 5,
                args: vec![0],
            },
            //  in n + sum
            Inst::IntAdd(5, 0),
            Inst::Ret,
            //  True -> 0
            Inst::Int(0),
            Inst::Ret,
        ];
        assert_eq!(eval(&prog).to_data_value(), DataValue::Int(15));
    }

    // main = let n = 15
    //         in fib n
    // fib = n ->
    //   let n=0 = n == 0
    //    in case n=0 of
    //        False -> let n=1 = n == 1
    //                  in case n=1 of
    //                       False -> let l1 = Cons 1 Nil
    //                                    prefix = Cons 1 l1
    //                                    fibs = fibBuild n prefix
    //                                 in case fibs of
    //                                     Nil -> panic
    //                                     Cons x xs -> x
    //                      True -> 1
    //        True -> 1
    //
    // fibBuild = n ms ->
    //   let msLen = length ms
    //       n<=msLen = n <= msLen
    //    in case n<=msLen of
    //         False -> let prefix = fib' ms
    //                   in fibBuild n prefix
    //         True -> ms
    //
    // fib' = ms ->
    //   case ms of
    //     Nil -> panic
    //     Cons x ms' -> case ms' of
    //                     Nil -> panic
    //                     Cons y ms'' -> let r = x + y
    //                                        l1 = Cons y ms''
    //                                        l2 = Cons x l1
    //                                     in Cons r l2
    // length = l ->
    //   case l of
    //     Nil -> 0
    //     Cons x xs -> let xsLen = length xs
    //                   in xsLen + 1
    // lteq = x y ->
    //   let x>y = x > y
    //    in not x>y
    //
    // not = b ->
    //   case b of
    //     True -> False
    //     False -> True
    #[test]
    fn test_10() {
        let prog: Vec<Inst> = vec![
            // call to main
            Inst::CallC {
                arity: 0,
                func_addr: 2,
                args: vec![],
            },
            Inst::Halt,
            // main = let n = 15                 [2]
            Inst::Int(15),
            // in fib n
            Inst::CallC {
                arity: 1,
                func_addr: 5,
                args: vec![0],
            },
            Inst::Ret,
            // fib = n ->                        [5]
            //   let n=0 = n == 0
            Inst::Int(0),
            Inst::IntEq(1, 0),
            // in case n=0 of
            Inst::Case(0, vec![8, 23]),
            // False -> let n=1 = n == 1
            Inst::Int(1),
            Inst::IntEq(3, 0),
            // in case n=1 of
            Inst::Case(0, vec![11, 21]),
            // False -> let l1 = Cons 1 Nil
            Inst::Int(1),
            Inst::Ctor(0, vec![]),
            Inst::Ctor(1, vec![1, 0]),
            // prefix = Cons 1 l1
            Inst::Int(1),
            Inst::Ctor(1, vec![0, 1]),
            // fibs = fibBuild n prefix
            Inst::CallC {
                arity: 2,
                func_addr: 25,
                args: vec![9, 0],
            },
            // case fibs of
            Inst::Case(0, vec![18, 19]),
            // Nil -> panic
            Inst::Panic,
            // Cons x xs -> x
            Inst::Var(1),
            Inst::Ret,
            // True -> 1
            Inst::Int(1),
            Inst::Ret,
            // True -> 1
            Inst::Int(1),
            Inst::Ret,
            // fibBuild = n ms ->                [25]
            //   let msLen = length ms
            Inst::CallC {
                arity: 1,
                func_addr: 42,
                args: vec![0],
            },
            //   n<=msLen = n <= msLen
            Inst::CallC {
                arity: 2,
                func_addr: 49,
                args: vec![2, 0],
            },
            //   case n<=msLen of
            Inst::Case(0, vec![28, 31]),
            //   False -> let prefix = fib' ms
            Inst::CallC {
                arity: 1,
                func_addr: 33,
                args: vec![2],
            },
            // in fibBuild n prefix
            Inst::CallC {
                arity: 2,
                func_addr: 25,
                args: vec![4, 0],
            },
            Inst::Ret,
            // True -> ms
            Inst::Var(2),
            Inst::Ret,
            // fib' = ms ->                      [33]
            //  case ms of
            Inst::Case(0, vec![34, 35]),
            // Nil -> panic
            Inst::Panic,
            // Cons x ms' -> case ms' of
            Inst::Case(0, vec![36, 37]),
            // Nil -> panic
            Inst::Panic,
            // Cons y ms'' ->
            //   let r = x + y
            Inst::IntAdd(3, 1),
            //       l1 = Cons y ms''
            Inst::Ctor(1, vec![2, 1]),
            //       l2 = Cons x l1
            Inst::Ctor(1, vec![5, 0]),
            //   in Cons r l2
            Inst::Ctor(1, vec![2, 0]),
            Inst::Ret,
            // length = l ->                     [42]
            //  case l of
            Inst::Case(0, vec![43, 45]),
            // Nil -> 0
            Inst::Int(0),
            Inst::Ret,
            // Cons x xs -> let xsLen = length xs
            Inst::CallC {
                arity: 1,
                func_addr: 42,
                args: vec![0],
            },
            //  in xsLen + 1
            Inst::Int(1),
            Inst::IntAdd(1, 0),
            Inst::Ret,
            // lteq = x y ->                     [49]
            // let x>y = x > y
            Inst::IntGt(1, 0),
            // in not x>y
            Inst::CallC {
                arity: 1,
                func_addr: 52,
                args: vec![0],
            },
            Inst::Ret,
            // not = b ->                        [52]
            //   case b of
            Inst::Case(0, vec![53, 55]),
            //     False -> True
            Inst::Ctor(1, vec![]),
            Inst::Ret,
            //     True -> False
            Inst::Ctor(0, vec![]),
            Inst::Ret,
        ];
        assert_eq!(eval(&prog).to_data_value(), DataValue::Int(610));
    }
}

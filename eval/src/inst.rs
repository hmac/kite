use std::ops::Index;
use std::rc::Rc;

/// Instructions for the Kite VM.
/// The Kite VM has a single stack which stores all variables and intermediate results.
#[derive(Debug)]
pub enum Inst {
    // Push a local variable onto the stack
    Var(StackAddr),
    // Push a function argument onto the stack.
    // Arguments are 0-indexed in reverse, e.g. f x y -> ... has arguments 0: y, 1: x.
    Func { arity: usize, addr: InstAddr },

    Int(i32),
    Call,
    // TODO: Case expressions on integer literals
    Case(StackAddr, Vec<InstAddr>),
    Ctor(u8, Vec<StackAddr>),

    // stack operations
    Ret,
    Halt,

    // primitive operations
    IntAdd(IntArg, IntArg),
    IntSub(IntArg, IntArg),
    IntMul(IntArg, IntArg),
    IntEq(IntArg, IntArg),
    IntLt(IntArg, IntArg),
    IntGt(IntArg, IntArg),
    Panic,
}

/// The stack. This holds function arguments, local variables and intermediate results.
#[derive(Debug)]
struct Stack<T> {
    inner: Vec<T>,
    // (start address of frame in stack, number of args remaining in call)
    frames: Vec<(usize, usize)>,
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

    fn current_frame(&self) -> (usize, usize) {
        *self.frames.last().unwrap()
    }

    /// Remove all elements up to the last stack frame
    fn pop_frame(&mut self) -> (usize, usize) {
        let (frame, args) = self.frames.pop().unwrap();
        self.inner.truncate(frame);
        (frame, args)
    }

    /// Push a new stack frame.
    /// This saves a marker into the current top of the stack so it can be restored later.
    /// It also saves the number of arguments remaining after this function call.
    fn push_frame(&mut self, marker: usize, num_args: usize) {
        self.frames.push((marker, num_args));
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
    Func { arity: usize, addr: I },
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
        addr: I,
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

/// An argument to an integer operation such as IntAdd.
#[derive(Debug, Clone, Copy)]
pub enum IntArg {
    Int(i32),
    Var(StackAddr),
}

impl IntArg {
    fn try_to_int<I>(&self, stack: &Stack<StackValue<I>>) -> Option<i32> {
        match self {
            IntArg::Int(i) => Some(*i),
            IntArg::Var(v) => match lookup_stack_addr(stack, *v) {
                StackValue::Int(i) => Some(*i),
                _ => None,
            },
        }
    }
}

type InstAddr = usize;

#[derive(Debug, Clone, Copy)]
pub enum StackAddr {
    Local(usize),
    Arg(usize),
}

/// Evaluate a sequence of instructions.
pub fn eval(insts: &[Inst]) -> StackValue<InstAddr> {
    let mut stack: Stack<StackValue<InstAddr>> = Stack::new();
    let mut call_stack: Stack<InstAddr> = Stack::new();
    let mut inst_addr = 0;
    let mut bail_counter = 0;
    while inst_addr < insts.len() && bail_counter < 2000 {
        bail_counter += 1;
        println!("stack: {:?}", stack.inner);
        println!("frames: {:?}", stack.frames);
        println!("inst({}): {:?}", inst_addr, insts[inst_addr]);
        println!("");
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

fn lookup_stack_addr<I>(stack: &Stack<StackValue<I>>, addr: StackAddr) -> &StackValue<I> {
    match addr {
        StackAddr::Local(i) => &stack[i],
        StackAddr::Arg(i) => {
            let (frame, _) = stack.current_frame();
            let arg_offset_from_frame = frame + i;
            let arg_offset_from_top = stack.inner.len() - 1 - arg_offset_from_frame;
            &stack[arg_offset_from_top]
        }
    }
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
            stack.push(lookup_stack_addr(stack, *v).clone());
        }
        Inst::Func { arity, addr } => {
            stack.push(StackValue::Func {
                arity: *arity,
                addr: *addr,
            });
        }
        Inst::Int(n) => {
            stack.push(StackValue::Int(*n));
        }
        // On entering this instruction, the stack will look like this:
        //
        // ------------
        // |   func   |
        // ------------
        // | num_args |
        // ------------
        // |   arg1   |
        // ------------
        // |   ...    |
        // ------------
        // |   argN   |
        // ------------
        //
        // If num_args == 0 then we might be done with evaluation. We just need to check if func is
        // a function or PAp with arity == 0, and then we have to call it. Otherwise, we can move
        // to the next instruction.
        //
        // If num_args > 0, we compare func.arity with num_args and either call func with some of
        // the args or create a PAp.
        // If we call func with fewer than num_args args, then we set the return address to this
        // instruction. We calculate how many args will remain after the call and save this value
        // in the stack frame. Ret will ensure that when we return, this value is returned to the
        // stack underneath the result of the function call, so the stack looks the same as it did
        // when we first entered this call instruction.
        //
        // This process means we don't need an EndOfArgs marker on each function call - we just
        // need to push an int indicating the number of args. Because this is pushed after the args
        // and dropped before the function call, it doesn't affect any local variable addresses.
        Inst::Call => {
            let func = stack.pop(1);
            let num_args = match stack.pop(1) {
                StackValue::Int(n) => n as usize,
                _ => panic!("Expected num_args to be an integer"),
            };
            match func {
                StackValue::Func { arity, addr } => {
                    if num_args == 0 && arity > 0 {
                        // func is a result in normal form, so we don't need to call it.
                        stack.push(func);
                        return Some(inst_addr + 1);
                    }
                    if num_args < arity {
                        // Construct a PAp
                        let args = {
                            let mut args = Vec::with_capacity(num_args);
                            for _ in 0..num_args {
                                args.push(stack.pop(1));
                            }
                            args
                        };
                        let pap = HeapValue::PAp { addr, arity, args };
                        stack.push(StackValue::Ref(Rc::new(pap)));
                    } else {
                        // num_args >= arity.
                        // Push a stack frame that includes <arity> args. We store in the stack
                        // frame (2nd elem) the number of args that still need to be consumed
                        // after this function returns.
                        stack.push_frame(stack.inner.len() - arity, num_args - arity);
                        // Push the current instruction as the return address
                        call_stack.push(inst_addr);
                        // Jump to function
                        return Some(addr);
                    }
                }
                StackValue::Ref(ref v) => {
                    match &**v {
                        HeapValue::PAp {
                            addr,
                            arity,
                            args: existing_args,
                        } => {
                            // If we have enough args now, then call the function.
                            // Push existing_args onto stack
                            let num_all_args = num_args + existing_args.len();
                            if *arity <= num_all_args {
                                for arg in existing_args.clone().into_iter() {
                                    stack.push(arg);
                                }
                                // Push num_args onto the stack
                                stack.push(StackValue::Int(num_all_args as i32));
                                // Push function onto stack
                                stack.push(StackValue::Func {
                                    arity: *arity,
                                    addr: *addr,
                                });
                                // Jump to current instruction to execute the call
                                return Some(inst_addr);
                            } else {
                                // Otherwise construct a new PAp containing all the args
                                let mut all_args = existing_args.clone();
                                for _ in 0..num_args {
                                    all_args.push(stack.pop(1));
                                }
                                let pap = HeapValue::PAp {
                                    addr: *addr,
                                    arity: *arity,
                                    args: all_args,
                                };
                                stack.push(StackValue::Ref(Rc::new(pap)));
                            }
                        }
                        _ => {
                            // If num_args == 0, we have a result.
                            // Push this onto the stack and jump to the next instruction.
                            // If num_args > 0, we have a bug, so panic.
                            if num_args == 0 {
                                stack.push(func);
                                return Some(inst_addr + 1);
                            } else {
                                panic!("cannot call a non-function: {:?}", func);
                            }
                        }
                    }
                }
                _ => {
                    // If num_args == 0, we have a result.
                    // Push this onto the stack and jump to the next instruction.
                    // If num_args > 0, we have a bug, so panic.
                    if num_args == 0 {
                        stack.push(func);
                        return Some(inst_addr + 1);
                    } else {
                        panic!("cannot call a non-function: {:?}", func);
                    }
                }
            }
        }
        Inst::Case(target, alts) => {
            let target = lookup_stack_addr(stack, *target).clone();
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
            let arg_vec: Vec<StackValue<InstAddr>> = args
                .iter()
                .map(|arg| lookup_stack_addr(stack, *arg).clone())
                .collect();
            let val = HeapValue::Ctor(*tag, arg_vec);
            stack.push(StackValue::Ref(Rc::new(val)));
        }

        Inst::Ret => {
            // Save the result
            let r = stack.pop(1);
            // Pop the call stack
            let (_, args_remaining) = stack.pop_frame();
            // Push the number of remaining args onto the stack (Call expects this).
            stack.push(StackValue::Int(args_remaining as i32));
            // Push the result back onto the stack
            stack.push(r);
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
    a: IntArg,
    b: IntArg,
    msg: &str,
    op: fn(i32, i32) -> StackValue<I>,
    stack: &Stack<StackValue<I>>,
) -> StackValue<I> {
    let a_val = a
        .try_to_int(stack)
        .unwrap_or_else(|| panic!("cannot {} non-integers", msg));
    let b_val = b
        .try_to_int(stack)
        .unwrap_or_else(|| panic!("cannot {} non-integers", msg));
    op(a_val, b_val)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_1() {
        let insts: Vec<Inst> = vec![Inst::Int(1), Inst::Var(StackAddr::Local(0))];
        assert_eq!(eval(&insts).to_data_value(), DataValue::Int(1));
    }

    #[test]
    fn test_2() {
        let insts: Vec<Inst> = vec![
            Inst::Int(1),
            Inst::Int(2),
            Inst::Ctor(0, vec![StackAddr::Local(1), StackAddr::Local(0)]),
        ];
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
            Inst::Int(0),
            Inst::Func { arity: 0, addr: 4 },
            Inst::Call,
            Inst::Halt,
            // main
            Inst::Int(1),
            Inst::Int(2),
            Inst::Ctor(1, vec![StackAddr::Local(1), StackAddr::Local(0)]),
            Inst::Case(StackAddr::Local(0), vec![8, 10]),
            Inst::Int(4),
            Inst::Ret,
            Inst::Int(5),
            Inst::Int(6),
            Inst::IntAdd(
                IntArg::Var(StackAddr::Local(0)),
                IntArg::Var(StackAddr::Local(1)),
            ),
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
            Inst::Int(0),
            Inst::Func { arity: 0, addr: 4 },
            Inst::Call,
            Inst::Halt,
            // main =
            //   let false = False in
            Inst::Ctor(0, vec![]),
            //     not false
            Inst::Int(1),
            Inst::Func { arity: 1, addr: 9 },
            Inst::Call,
            Inst::Ret,
            // not = b -> case b of
            Inst::Case(StackAddr::Arg(0), vec![10, 12]),
            //     False -> True
            Inst::Ctor(1, vec![]),
            Inst::Ret,
            //     True -> False
            Inst::Ctor(0, vec![]),
            Inst::Ret,
        ];
        assert_eq!(eval(&prog).to_data_value(), DataValue::Ctor(1, vec![]));
    }

    // fromMaybe(m, d) = case m of { Just x -> x; Nothing -> d }
    // main = let one = 1 in let two = 2 in let r = Just two in fromMaybe r one
    #[test]
    fn test_5() {
        let prog: Vec<Inst> = vec![
            // call to main
            Inst::Int(0),
            Inst::Func { arity: 0, addr: 4 },
            Inst::Call,
            Inst::Halt,
            // [4] main =
            //   let one = 1
            Inst::Int(1),
            //       two = 2
            Inst::Int(2),
            //       r = Just two
            Inst::Ctor(1, vec![StackAddr::Local(0)]),
            //   in fromMaybe r one
            Inst::Var(StackAddr::Local(2)),
            Inst::Var(StackAddr::Local(1)),
            Inst::Int(2),
            Inst::Func { arity: 2, addr: 13 },
            Inst::Call,
            Inst::Ret,
            // [13] fromMaybe = m d ->
            //   case m of
            Inst::Case(StackAddr::Arg(1), vec![14, 16]),
            //     Nothing -> d
            Inst::Var(StackAddr::Arg(0)),
            Inst::Ret,
            //     Just x -> x
            Inst::Var(StackAddr::Local(0)),
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
            Inst::Int(0),
            Inst::Func { arity: 0, addr: 4 },
            Inst::Call,
            Inst::Halt,
            // [4] main =
            //  let nil = Nil
            Inst::Ctor(0, vec![]),
            //      true = True
            Inst::Ctor(1, vec![]),
            //      false = False
            Inst::Ctor(0, vec![]),
            //      l1 = Cons false nil
            Inst::Ctor(1, vec![StackAddr::Local(0), StackAddr::Local(2)]),
            //      l2 = Cons true l1
            Inst::Ctor(1, vec![StackAddr::Local(2), StackAddr::Local(0)]),
            //      not = [global not]
            Inst::Int(0),
            Inst::Func { arity: 1, addr: 16 },
            Inst::Call,
            //   in map not l2
            Inst::Int(2),
            Inst::Func { arity: 2, addr: 21 },
            Inst::Call,
            Inst::Ret,
            // [16] not = b ->
            //   case b of
            Inst::Case(StackAddr::Arg(0), vec![17, 19]),
            //     False -> True
            Inst::Ctor(1, vec![]),
            Inst::Ret,
            //     True -> False
            Inst::Ctor(0, vec![]),
            Inst::Ret,
            // map = f l ->
            //   case l of
            Inst::Case(StackAddr::Arg(0), vec![22, 24]),
            //     Nil -> Nil
            Inst::Ctor(0, vec![]),
            Inst::Ret,
            //     Cons x xs ->
            //       let x' = f x
            Inst::Var(StackAddr::Local(1)),
            Inst::Int(1),
            Inst::Var(StackAddr::Arg(1)),
            Inst::Call,
            //           xs' = map f xs
            Inst::Var(StackAddr::Local(1)),
            Inst::Var(StackAddr::Arg(1)),
            Inst::Int(2),
            Inst::Func { arity: 2, addr: 21 },
            Inst::Call,
            //        in Cons x' xs'
            Inst::Ctor(1, vec![StackAddr::Local(1), StackAddr::Local(0)]),
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
            Inst::Int(0),
            Inst::Func { arity: 0, addr: 4 },
            Inst::Call,
            Inst::Halt,
            // [4] main = let p = pair True False
            Inst::Ctor(0, vec![]),
            Inst::Ctor(1, vec![]),
            Inst::Int(2),
            Inst::Func { arity: 2, addr: 13 },
            Inst::Call,
            //   in swap p
            Inst::Int(1),
            Inst::Func { arity: 1, addr: 15 },
            Inst::Call,
            Inst::Ret,
            // [13] pair = x y -> Pair x y
            Inst::Ctor(0, vec![StackAddr::Arg(1), StackAddr::Arg(0)]),
            Inst::Ret,
            // [15] swap = p ->
            // case p of
            Inst::Case(StackAddr::Arg(0), vec![16]),
            //     Pair x y -> Pair y x
            Inst::Ctor(0, vec![StackAddr::Local(0), StackAddr::Local(1)]),
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
            Inst::Int(0),
            Inst::Func { arity: 0, addr: 4 },
            Inst::Call,
            Inst::Halt,
            // main = map inc list
            Inst::Int(0),
            Inst::Func { arity: 0, addr: 14 }, // list
            Inst::Call,
            Inst::Int(0),
            Inst::Func { arity: 1, addr: 24 }, // inc
            Inst::Call,
            Inst::Int(2),
            Inst::Func { arity: 2, addr: 26 }, // map
            Inst::Call,
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
            Inst::Ctor(1, vec![StackAddr::Local(0), StackAddr::Local(4)]),
            //       l2 = Cons x2 l1
            Inst::Ctor(1, vec![StackAddr::Local(2), StackAddr::Local(0)]),
            //       l3 = Cons x1 l2
            Inst::Ctor(1, vec![StackAddr::Local(4), StackAddr::Local(0)]),
            //       l4 = Cons x0 l3
            Inst::Ctor(1, vec![StackAddr::Local(6), StackAddr::Local(0)]),
            //       in l4
            Inst::Ret,
            // inc = x -> x + 1
            Inst::IntAdd(IntArg::Var(StackAddr::Arg(0)), IntArg::Int(1)),
            Inst::Ret,
            // map = f l ->
            //   case l of
            Inst::Case(StackAddr::Arg(0), vec![27, 29]),
            //     Nil -> Nil
            Inst::Ctor(0, vec![]),
            Inst::Ret,
            //     Cons x xs ->
            //       let x' = f x
            Inst::Var(StackAddr::Local(1)),
            Inst::Int(1),
            Inst::Var(StackAddr::Arg(1)),
            Inst::Call,
            //           xs' = map f xs
            Inst::Var(StackAddr::Local(1)),
            Inst::Var(StackAddr::Arg(1)),
            Inst::Int(2),
            Inst::Func { arity: 2, addr: 26 },
            Inst::Call,
            //        in Cons x' xs'
            Inst::Ctor(1, vec![StackAddr::Local(1), StackAddr::Local(0)]),
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
            Inst::Int(0),
            Inst::Func { arity: 0, addr: 4 },
            Inst::Call,
            Inst::Halt,
            // [4] main = sum_to 5
            Inst::Int(5),
            Inst::Int(1),
            Inst::Func { arity: 1, addr: 9 },
            Inst::Call,
            Inst::Ret,
            // [9] sum_to = n -> let neq0 = n == 0
            Inst::IntEq(IntArg::Var(StackAddr::Arg(0)), IntArg::Int(0)),
            // in case neq0 of
            Inst::Case(StackAddr::Local(0), vec![11, 18]),
            //  False -> let n-1 = n - 1
            Inst::IntSub(IntArg::Var(StackAddr::Arg(0)), IntArg::Int(1)),
            //  sum = sum_to n-1
            Inst::Var(StackAddr::Local(0)),
            Inst::Int(1),
            Inst::Func { arity: 1, addr: 9 },
            Inst::Call,
            //  in n + sum
            Inst::IntAdd(
                IntArg::Var(StackAddr::Arg(0)),
                IntArg::Var(StackAddr::Local(0)),
            ),
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
            Inst::Int(0),
            Inst::Func { arity: 0, addr: 4 },
            Inst::Call,
            Inst::Halt,
            // [4] main = fib 15
            Inst::Int(15),
            Inst::Int(1),
            Inst::Func { arity: 1, addr: 9 },
            Inst::Call,
            Inst::Ret,
            // [9] fib = n -> let n=0 = n == 0
            Inst::IntEq(IntArg::Var(StackAddr::Arg(0)), IntArg::Int(0)),
            // in case n=0 of
            Inst::Case(StackAddr::Local(0), vec![11, 29]),
            // False -> let n=1 = n == 1
            Inst::IntEq(IntArg::Var(StackAddr::Arg(0)), IntArg::Int(1)),
            // in case n=1 of
            Inst::Case(StackAddr::Local(0), vec![13, 27]),
            // False -> let l1 = Cons 1 Nil
            Inst::Int(1),
            Inst::Ctor(0, vec![]),
            Inst::Ctor(1, vec![StackAddr::Local(1), StackAddr::Local(0)]),
            // prefix = Cons 1 l1
            Inst::Int(1),
            Inst::Ctor(1, vec![StackAddr::Local(0), StackAddr::Local(1)]),
            // fibs = fibBuild n prefix
            Inst::Var(StackAddr::Local(0)),
            Inst::Var(StackAddr::Arg(0)),
            Inst::Int(2),
            Inst::Func { arity: 2, addr: 31 },
            Inst::Call,
            // case fibs of
            Inst::Case(StackAddr::Local(0), vec![24, 25]),
            // Nil -> panic
            Inst::Panic,
            // Cons x xs -> x
            Inst::Var(StackAddr::Local(1)),
            Inst::Ret,
            // True -> 1
            Inst::Int(1),
            Inst::Ret,
            // True -> 1
            Inst::Int(1),
            Inst::Ret,
            // [31] fibBuild = n ms -> let msLen = length ms
            Inst::Var(StackAddr::Arg(0)),
            Inst::Int(1),
            Inst::Func { arity: 1, addr: 62 },
            Inst::Call,
            //   n<=msLen = lteq n msLen
            Inst::Var(StackAddr::Local(0)),
            Inst::Var(StackAddr::Arg(1)),
            Inst::Int(2),
            Inst::Func { arity: 2, addr: 72 },
            Inst::Call,
            //   case n<=msLen of
            Inst::Case(StackAddr::Local(0), vec![41, 51]),
            //   False -> let prefix = fib' ms
            Inst::Var(StackAddr::Arg(0)),
            Inst::Int(1),
            Inst::Func { arity: 1, addr: 53 },
            Inst::Call,
            // in fibBuild n prefix
            Inst::Var(StackAddr::Local(0)),
            Inst::Var(StackAddr::Arg(1)),
            Inst::Int(2),
            Inst::Func { arity: 2, addr: 31 },
            Inst::Call,
            Inst::Ret,
            // True -> ms
            Inst::Var(StackAddr::Arg(0)),
            Inst::Ret,
            // [53] fib' = ms -> case ms of
            Inst::Case(StackAddr::Arg(0), vec![54, 55]),
            // Nil -> panic
            Inst::Panic,
            // Cons x ms' -> case ms' of
            Inst::Case(StackAddr::Local(0), vec![56, 57]),
            // Nil -> panic
            Inst::Panic,
            // Cons y ms'' ->
            //   let r = x + y
            Inst::IntAdd(
                IntArg::Var(StackAddr::Local(3)),
                IntArg::Var(StackAddr::Local(1)),
            ),
            //       l1 = Cons y ms''
            Inst::Ctor(1, vec![StackAddr::Local(2), StackAddr::Local(1)]),
            //       l2 = Cons x l1
            Inst::Ctor(1, vec![StackAddr::Local(5), StackAddr::Local(0)]),
            //   in Cons r l2
            Inst::Ctor(1, vec![StackAddr::Local(2), StackAddr::Local(0)]),
            Inst::Ret,
            // [62] length = l -> case l of
            Inst::Case(StackAddr::Arg(0), vec![63, 65]),
            // Nil -> 0
            Inst::Int(0),
            Inst::Ret,
            // Cons x xs -> let xsLen = length xs
            Inst::Var(StackAddr::Local(0)),
            Inst::Int(1),
            Inst::Func { arity: 1, addr: 62 },
            Inst::Call,
            //  in xsLen + 1
            Inst::Int(1),
            Inst::IntAdd(
                IntArg::Var(StackAddr::Local(1)),
                IntArg::Var(StackAddr::Local(0)),
            ),
            Inst::Ret,
            // [72] lteq = x y -> let x>y = x > y
            Inst::IntGt(
                IntArg::Var(StackAddr::Arg(1)),
                IntArg::Var(StackAddr::Arg(0)),
            ),
            // in not x>y
            Inst::Var(StackAddr::Local(0)),
            Inst::Int(1),
            Inst::Func { arity: 1, addr: 78 },
            Inst::Call,
            Inst::Ret,
            // [78] not = b -> case b of
            Inst::Case(StackAddr::Arg(0), vec![79, 81]),
            //     False -> True
            Inst::Ctor(1, vec![]),
            Inst::Ret,
            //     True -> False
            Inst::Ctor(0, vec![]),
            Inst::Ret,
        ];
        assert_eq!(eval(&prog).to_data_value(), DataValue::Int(610));
    }

    // main = fib 15
    // fib n = go n 1 1
    // go n x y =
    //   let n<3 = n < 3
    //    in case n<3 of
    //         False ->
    //           let z = x + y
    //               m = n - 1
    //            in go m z x
    //         True -> x
    //
    #[test]
    fn test_11() {
        let prog: Vec<Inst> = vec![
            // call to main
            Inst::Int(0),
            Inst::Func { arity: 0, addr: 4 },
            Inst::Call,
            Inst::Halt,
            // [4] main = fib 15
            Inst::Int(15),
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
        ];
        assert_eq!(eval(&prog).to_data_value(), DataValue::Int(610));
    }

    // A test for function calls with too many arguments.
    // returnAdd takes one argument and returns a function that takes two arguments.
    // We give returnAdd three arguments. The first is used in the evaluation of returnAdd, and the
    // other two are used in the evaluation of the returned function.
    //
    // add = x y -> x + y
    // returnAdd = z -> add
    // main = returnAdd 1 2 3
    #[test]
    fn test_12() {
        let prog: Vec<Inst> = vec![
            // call to main
            Inst::Int(0),
            Inst::Func { arity: 0, addr: 4 },
            Inst::Call,
            Inst::Halt,
            // [4] main = returnAdd 1 2 3
            Inst::Int(3),
            Inst::Int(2),
            Inst::Int(1),
            Inst::Int(3),
            Inst::Func { arity: 1, addr: 11 },
            Inst::Call,
            Inst::Ret,
            // [11] returnAdd = z -> add
            Inst::Int(0),
            Inst::Func { arity: 2, addr: 15 },
            Inst::Call,
            Inst::Ret,
            // [15] add = x y -> x + y
            Inst::IntAdd(
                IntArg::Var(StackAddr::Arg(0)),
                IntArg::Var(StackAddr::Arg(1)),
            ),
            Inst::Ret,
        ];
        assert_eq!(eval(&prog).to_data_value(), DataValue::Int(5));
    }

    // A test for function calls with too few arguments.
    //
    // main = let f = add 1
    //         in apply f 2
    // apply = f x -> f x
    // add = x y -> x + y
    #[test]
    fn test_13() {
        let prog: Vec<Inst> = vec![
            // call to main
            Inst::Int(0),
            Inst::Func { arity: 0, addr: 4 },
            Inst::Call,
            Inst::Halt,
            // [4] main = let f = add 1
            Inst::Int(1),
            Inst::Int(1),
            Inst::Func { arity: 2, addr: 19 },
            Inst::Call,
            // in apply f 2
            Inst::Int(2),
            Inst::Var(StackAddr::Local(1)),
            Inst::Int(2),
            Inst::Func { arity: 2, addr: 14 },
            Inst::Call,
            Inst::Ret,
            // [14] apply = f x -> f x
            Inst::Var(StackAddr::Arg(0)),
            Inst::Int(1),
            Inst::Var(StackAddr::Arg(1)),
            Inst::Call,
            Inst::Ret,
            // [19] add = x y -> x + y
            Inst::IntAdd(
                IntArg::Var(StackAddr::Local(0)),
                IntArg::Var(StackAddr::Local(1)),
            ),
            Inst::Ret,
        ];
        assert_eq!(eval(&prog).to_data_value(), DataValue::Int(3));
    }
}

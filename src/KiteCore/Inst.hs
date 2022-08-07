module KiteCore.Inst where

import Data.Word (Word8)

-- | This is a copy of the 'Inst' type from the Rust codebase.
data Inst a
  = Var StackAddr
  | Func {arity :: Int, addr :: a}
  | Int Int
  | Call
  | Ret
  | Case StackAddr [(Word8, a)]
  | CaseInt StackAddr a [(Int, a)]
  | Ctor Word8 [StackAddr]
  | Halt
  | IntAdd IntArg IntArg
  | IntSub IntArg IntArg
  | IntMul IntArg IntArg
  | IntEq IntArg IntArg
  | IntLt IntArg IntArg
  | IntGt IntArg IntArg
  | Panic

data StackAddr = Local Int | Arg Int

data IntArg = IntArg Int | VarArg StackAddr

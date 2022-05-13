module Eval where
-- An interpreter for KiteCore.
-- KiteCore is a small functional language that we can compile Kite into.
-- It can be interpreted using a model similar to the STG machine (but without
-- all the lazy stuff).

import           Data.List                      ( find
                                                , findIndex
                                                , mapAccumL
                                                )
import           Data.Maybe                     ( fromJust )

-- First we need to define KiteCore

-- A KiteCore program is a series of top-level function definitions
type Prog var = [Def var]

-- A KiteCore function definition has a name, some arguments, and an expression.
newtype Global = Global String deriving (Eq, Show)
data Def var = Def
  { defName   :: Global
  , defArity  :: Int          -- defArity == length . defParams
  , defParams :: [String]
  , defExpr   :: KExpr var
  }
  deriving Show

-- Variables refer to:
-- - global top-level function definitions
-- - parameters (args) bound by the current function
-- - local variables introduced by lets
data NamedVar = NamedGlobalVar Global
              | NamedLocalVar String
              | NamedArgVar String
  deriving Show

-- A KiteCore expression
data KExpr var =
      KVar var                             -- variable
    | KApp var [var]                       -- function application
    | KLet String (KExpr var) (KExpr var)  -- let binding
    | KCase var [(Pat, KExpr var)]         -- case analysis
    | KCtor Ctor [var]                     -- constructor application (always saturated)
    | KInt Int                             -- integer literal
    | KPrim Prim [var]                     -- primitive functions
  deriving Show

data Pat = CtorPat Ctor [String]
  deriving Show

data Prim = PrimIntAdd
  deriving Show

-- Constructors are unique names with a natural number tag indicating their
-- order in their type.
data Ctor = Ctor String Int
  deriving Show

-- KiteCore values
-- We store KiteCore values in the heap. They are either constructors, or
-- partial applications of functions (and eventually, literals like integers).
data KVal =
    CtorVal Ctor [KVal]
  | IntVal Int
  | PAp (Def NamelessVar) [KVal]
  deriving Show

type GlobalEnv var = [Def var]

-- Before evaluating, we want to get rid of all variable names and replace them
-- with static addresses or offsets into one of the stacks.
-- Function args are converted into offsets in the arg stack.
-- Local vars are converted into offsets in the locals stack.
-- Global vars are converted into references to the corresponding global.
-- Since global definitions contain their function body which is itself an
-- expression, we must start with a 'NamedGlobalEnv' and gradually convert it to
-- a 'NamelessGlobalEnv'.

data NamelessVar =
      GlobalVar (Def NamelessVar)
    | LocalVar Int
    | ArgVar Int
  deriving Show

lookupGlobalVar :: Global -> GlobalEnv a -> Def a
lookupGlobalVar g = fromJust . find ((== g) . defName)

makeNamelessDef :: GlobalEnv NamelessVar -> Def NamedVar -> Def NamelessVar
makeNamelessDef globals def =
  let expr = makeNamelessExpr globals (defParams def) [] (defExpr def)
  in  def { defExpr = expr }

makeNamelessExpr
  :: GlobalEnv NamelessVar
  -> [String]
  -> [String]
  -> KExpr NamedVar
  -> KExpr NamelessVar
makeNamelessExpr genv args locals kexpr = case kexpr of
  KVar v    -> KVar $ makeNamelessVar genv args locals v
  KApp f xs -> KApp (makeNamelessVar genv args locals f)
    $ map (makeNamelessVar genv args locals) xs
  KLet x e1 e2 -> KLet x
                       (makeNamelessExpr genv args locals e1)
                       (makeNamelessExpr genv args (x : locals) e2)
  KCase v alts -> KCase (makeNamelessVar genv args locals v) $ map
    (\(CtorPat ctor vars, rhs) ->
      ( CtorPat ctor vars
      , makeNamelessExpr genv args (reverse vars ++ locals) rhs
      )
    )
    alts
  KCtor ctor xs -> KCtor ctor $ map (makeNamelessVar genv args locals) xs
  KPrim p    xs -> KPrim p $ map (makeNamelessVar genv args locals) xs
  KInt i        -> KInt i

makeNamelessVar
  :: GlobalEnv NamelessVar -> [String] -> [String] -> NamedVar -> NamelessVar
makeNamelessVar genv args locals var = case var of
  NamedGlobalVar g -> GlobalVar $ lookupGlobalVar g genv
  NamedArgVar    v -> ArgVar $ fromJust $ findIndex (== v) args
  NamedLocalVar  v -> LocalVar $ fromJust $ findIndex (== v) locals

-- We evaluate an expression in the context of:
-- - The global environment, mapping globals to definitions
-- - The heap, mapping addresses to values
-- - The argument stack, holding addresses to function arguments (in the heap)
-- - The locals stack, holding addresses to values bound by lets
-- - The special "return value" register, which holds the last eval result


-- The heap should really be represented as a vector or set of reference cells,
-- or something.
-- We currently don't do any garbage collection, because ultimately we want to
-- introduce automatic reference counting.
type Heap = [KVal]

-- An address in the heap
type Addr = Int
type ArgStack = [Addr]
type LocalStack = [Addr]
type RetVal = Addr

eval
  :: (GlobalEnv NamelessVar, Heap, ArgStack, LocalStack)
  -> KExpr NamelessVar
  -> (Heap, KVal)
eval (env, heap, args, locals) kexpr = case kexpr of
  KInt i    -> (heap, IntVal i)
  KVar v    -> lookupVar env heap args locals v
  KApp f xs -> evalApp env heap args locals f xs
  KLet _ e1 e2 ->
    let (heap', e1result) = eval (env, heap, args, locals) e1
        heap''            = heap' ++ [e1result]
    in  seq heap'' $ eval (env, heap'', args, length heap' : locals) e2
  KCase v alts ->
    -- Eval v
    let (heap', vresult) = eval (env, heap, args, locals) (KVar v)
    -- Examine result, check tag, eval alt with same tag (bind ctor args)
    in  case vresult of
          PAp _ _  -> error "Case analysis on non-constructor (PAp)"
          IntVal _ -> error "Case analysis on non-constructor (int)"
          CtorVal (Ctor _ tag) ctorArgs ->
            -- Find alt with same tag
            case find (\(CtorPat (Ctor _ t) _, _) -> t == tag) alts of
              Just (CtorPat _ _, rhs) ->
                -- Bind args to vars
                -- Store each arg in the heap (so we have an addres for it)
                let heap''    = heap ++ ctorArgs
                    ctorAddrs = take (length ctorArgs) [length heap' ..]
                -- We must bind in reverse to be consistent with how lets are
                -- bound (outermost first).
                    locals'   = reverse ctorAddrs ++ locals
                -- Eval rhs
                in  eval (env, heap'', args, locals') rhs
              Nothing ->
                error $ "no matching case alternative: " <> show vresult
  KCtor ctor ctorArgs ->
    -- Args are guaranteed to be in evaluated and on the heap already
    -- So we just look them up, construct the 'CtorVal' and store it in the
    -- heap, returning its address.
    let (heap', ctorArgVals) =
          mapAccumL (\h v -> lookupVar env h args locals v) heap ctorArgs
    in  (heap', seq heap' $ CtorVal ctor ctorArgVals)
  KPrim p xs -> evalPrim env heap args locals p xs

lookupVar
  :: GlobalEnv NamelessVar
  -> Heap
  -> ArgStack
  -> LocalStack
  -> NamelessVar
  -> (Heap, KVal)
lookupVar env heap args locals var = case var of
  GlobalVar def -> if defArity def == 0
    then evalApp' env heap args locals def [] []
    else (heap, PAp def [])
  LocalVar i -> (heap, heap !! (locals !! i))
  ArgVar   i -> (heap, heap !! (args !! i))

evalPrim
  :: GlobalEnv NamelessVar
  -> Heap
  -> ArgStack
  -> LocalStack
  -> Prim
  -> [NamelessVar]
  -> (Heap, KVal)
evalPrim env heap args locals prim xs = case (prim, xs) of
  (PrimIntAdd, [x, y]) ->
    let (heap' , xval) = lookupVar env heap args locals x
        (heap'', yval) = lookupVar env heap' args locals y
    in  case (xval, yval) of
          (IntVal xi, IntVal yi) ->
            let result  = IntVal (xi + yi)
                heap''' = heap'' ++ [result]
            in  (heap''', result)
          _ -> error $ "prim + applied to wrong type of args: " <> show [x, y]
  (PrimIntAdd, _) ->
    error $ "prim + applied to wrong number of args: " <> show xs
evalApp
  :: GlobalEnv NamelessVar
  -> Heap
  -> ArgStack
  -> LocalStack
  -> NamelessVar
  -> [NamelessVar]
  -> (Heap, KVal)
evalApp env heap args locals f xs =
  -- Lookup function. It must be a PAp.
  -- Check its arity, add args to it until we run out of args or the arity is
  -- met.
  -- If arity is met, push all args onto stack and evaluate body. 
  -- Otherwise, update heap with new PAp and return its address.
  let (heap', v) = lookupVar env heap args locals f
  in  case v of
        PAp     def args0 -> evalApp' env heap' args locals def args0 xs
        CtorVal _   _     -> error "Cannot apply non-function (ctor)"
        IntVal _          -> error "Cannot apply non-function (int)"

evalApp'
  :: GlobalEnv NamelessVar
  -> Heap
  -> ArgStack
  -> LocalStack
  -> Def NamelessVar
  -> [KVal]
  -> [NamelessVar]
  -> (Heap, KVal)
evalApp' env heap args locals def args0 xs =
  -- Check def arity, add args to it until we run out of args or the arity is
  -- met.
  -- If arity is met, push all args onto stack and evaluate body. 
  -- Otherwise, update heap with new PAp and return its address.
  let argsNeeded = defArity def - length args0
  in  if argsNeeded > length xs
        then
          let (heap', xsvals) =
                mapAccumL (\h v -> lookupVar env h args locals v) heap xs
              pap' = PAp def (args0 ++ xsvals)
          in  (heap' ++ [pap'], seq heap' $ pap')
        else
          let (heap', xsvals) = mapAccumL
                (\h v -> lookupVar env h args locals v)
                heap
                (take argsNeeded xs)
              allArgs     = args0 ++ xsvals
              heap''      = heap' ++ allArgs
              allArgAddrs = take (length allArgs) [length heap' ..]
          in  seq heap'' $ eval (env, heap'', allArgAddrs, locals) (defExpr def)

--- Examples

-- Cons Unit Nil
example1 :: KExpr NamedVar
example1 =
  KLet "nil" (KCtor (Ctor "Nil" 0) [])
    $ KLet "unit" (KCtor (Ctor "Unit" 0) [])
    $ KCtor (Ctor "Cons" 1) [NamedLocalVar "unit", NamedLocalVar "nil"]

-- id(x) = x
-- example1() = let nil = Nil in let unit = Unit in Cons unit nil
-- main() = id example1
example2 :: Prog NamedVar
example2 =
  [ Def { defName   = Global "id"
        , defArity  = 1
        , defParams = ["x"]
        , defExpr   = KVar (NamedArgVar "x")
        }
  , Def { defName   = Global "example1"
        , defArity  = 0
        , defParams = []
        , defExpr   = example1
        }
  , Def
    { defName   = Global "main"
    , defArity  = 0
    , defParams = []
    , defExpr   = KApp (NamedGlobalVar (Global "id"))
                       [NamedGlobalVar (Global "example1")]
    }
  ]

-- not(b) = case b of True -> False; False -> True
-- main = let false = False in not false
example3 :: Prog NamedVar
example3 =
  let false = Ctor "False" 0
      true  = Ctor "True" 1
  in  [ Def
        { defName   = Global "not"
        , defArity  = 1
        , defParams = ["b"]
        , defExpr   = KCase
                        (NamedArgVar "b")
                        [ (CtorPat true [] , KCtor false [])
                        , (CtorPat false [], KCtor true [])
                        ]
        }
      , Def
        { defName   = Global "main"
        , defArity  = 0
        , defParams = []
        , defExpr   = KLet
                        "false"
                        (KCtor false [])
                        (KApp (NamedGlobalVar (Global "not"))
                              [NamedLocalVar "false"]
                        )
        }
      ]

-- fromMaybe(m, d) = case m of { Just x -> x; Nothing -> d }
-- main = let nil = Nil in let unit = Unit in let r = Just unit in fromMaybe r nil
example4 :: Prog NamedVar
example4 =
  let unit    = Ctor "Unit" 0
      nil     = Ctor "Nil" 0
      nothing = Ctor "Nothing" 0
      just    = Ctor "Just" 1
  in  [ Def
        { defName   = Global "fromMaybe"
        , defArity  = 2
        , defParams = ["m", "d"]
        , defExpr   = KCase
                        (NamedArgVar "m")
                        [ (CtorPat just ["x"], KVar (NamedLocalVar "x"))
                        , (CtorPat nothing [], KVar (NamedArgVar "d"))
                        ]
        }
      , Def
        { defName   = Global "main"
        , defArity  = 0
        , defParams = []
        , defExpr   = KLet
                        "nil"
                        (KCtor nil [])
                        (KLet
                          "unit"
                          (KCtor unit [])
                          (KLet
                            "r"
                            (KCtor just [NamedLocalVar "unit"])
                            (KApp (NamedGlobalVar (Global "fromMaybe"))
                                  [NamedLocalVar "r", NamedLocalVar "nil"]
                            )
                          )
                        )
        }
      ]

exampleMap :: Def NamedVar
exampleMap =
  let nil  = Ctor "Nil" 0
      cons = Ctor "Cons" 1
  in  Def
        { defName   = Global "map"
        , defArity  = 2
        , defParams = ["f", "l"]
        , defExpr   = KCase
                        (NamedArgVar "l")
                        [ (CtorPat nil [], KCtor nil [])
                        , ( CtorPat cons ["x", "xs"]
                          , KLet
                            "x'"
                            (KApp (NamedArgVar "f") [NamedLocalVar "x"])
                            (KLet
                              "xs'"
                              (KApp (NamedGlobalVar (Global "map"))
                                    [NamedArgVar "f", NamedLocalVar "xs"]
                              )
                              (KCtor cons
                                     [NamedLocalVar "x'", NamedLocalVar "xs'"]
                              )
                            )
                          )
                        ]
        }

-- map(f, l) =
--   case l of {
--     Nil -> Nil
--     Cons x xs -> let x' = f x
--                   in let xs' = map f xs
--                       in Cons x' xs'
-- not(b) = case b of { True -> False; False -> True }
-- main = let nil = Nil
--         in let true = True
--             in let false = False
--                 in let l1 = Cons false nil
--                     in let l2 = Cons true l1
--                         in map not l2
example5 :: Prog NamedVar
example5 =
  let false = Ctor "False" 0
      true  = Ctor "True" 1
      nil   = Ctor "Nil" 0
      cons  = Ctor "Cons" 1
  in  [ Def
        { defName   = Global "not"
        , defArity  = 1
        , defParams = ["b"]
        , defExpr   = KCase
                        (NamedArgVar "b")
                        [ (CtorPat true [] , KCtor false [])
                        , (CtorPat false [], KCtor true [])
                        ]
        }
      , exampleMap
      , Def
        { defName   = Global "main"
        , defArity  = 0
        , defParams = []
        , defExpr   =
          KLet "nil" (KCtor nil [])
          $ KLet "true"  (KCtor true [])
          $ KLet "false" (KCtor false [])
          $ KLet "l1" (KCtor cons [NamedLocalVar "false", NamedLocalVar "nil"])
          $ KLet
              "l2"
              (KCtor cons [NamedLocalVar "true", NamedLocalVar "l1"])
              (KApp (NamedGlobalVar (Global "map"))
                    [NamedGlobalVar (Global "not"), NamedLocalVar "l2"]
              )
        }
      ]

example6 :: Prog NamedVar
example6 =
  let pair  = Ctor "Pair" 0
      true  = Ctor "True" 1
      false = Ctor "False" 0
  in  [ Def
        { defName   = Global "swap"
        , defArity  = 1
        , defParams = ["p"]
        , defExpr   = KCase
                        (NamedArgVar "p")
                        [ ( CtorPat pair ["x", "y"]
                          , KCtor pair [NamedLocalVar "y", NamedLocalVar "x"]
                          )
                        ]
        }
      , Def { defName   = Global "pair"
            , defArity  = 2
            , defParams = ["x", "y"]
            , defExpr   = KCtor pair [NamedArgVar "x", NamedArgVar "y"]
            }
      , Def
        { defName   = Global "main"
        , defArity  = 0
        , defParams = []
        , defExpr   = KLet "true" (KCtor true [])
                      $ KLet "false" (KCtor false [])
                      $ KLet
                          "p"
                          (KApp (NamedGlobalVar (Global "pair"))
                                [NamedLocalVar "true", NamedLocalVar "false"]
                          )
                      $ KApp (NamedGlobalVar (Global "swap")) [NamedLocalVar "p"]
        }
      ]

example7 :: Prog NamedVar
example7 =
  let
    nil  = Ctor "Nil" 0
    cons = Ctor "Cons" 1
  in
    [ Def
      { defName   = Global "main"
      , defArity  = 0
      , defParams = []
      , defExpr   = KApp
                      (NamedGlobalVar (Global "map"))
                      [ NamedGlobalVar (Global "inc")
                      , NamedGlobalVar (Global "list")
                      ]
      }
    , exampleMap
    , Def
      { defName   = Global "inc"
      , defArity  = 1
      , defParams = ["x"]
      , defExpr   = KLet "one" (KInt 1)
                      $ KPrim PrimIntAdd [NamedArgVar "x", NamedLocalVar "one"]
      }
    , Def
      { defName   = Global "list"
      , defArity  = 0
      , defParams = []
      , defExpr   =
        KLet "nil" (KCtor nil [])
        $ KLet "x0" (KInt 0)
        $ KLet "x1" (KInt 1)
        $ KLet "x2" (KInt 2)
        $ KLet "x3" (KInt 3)
        $ KLet "l1" (KCtor cons [NamedLocalVar "x3", NamedLocalVar "nil"])
        $ KLet "l2" (KCtor cons [NamedLocalVar "x2", NamedLocalVar "l1"])
        $ KLet "l3" (KCtor cons [NamedLocalVar "x1", NamedLocalVar "l2"])
        $ KLet "l4" (KCtor cons [NamedLocalVar "x0", NamedLocalVar "l3"])
        $ KVar (NamedLocalVar "l4")
      }
    ]

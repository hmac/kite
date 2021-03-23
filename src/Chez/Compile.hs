{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chez.Compile
  ( Env
  , compileModule
  , builtins
  ) where

import           Chez                           ( Def(..)
                                                , Lit(..)
                                                , SExpr(..)
                                                , begin
                                                , false
                                                , null
                                                , true
                                                )
import           Data.List.Extra                ( concatUnzip )
import           Data.Name                      ( Name(..)
                                                , RawName(..)
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified NameGen
import           NameGen                        ( NameGen
                                                , freshM
                                                )
import           Prelude                 hiding ( null )
import qualified Syn.Typed                     as T
import           Util

-- Compile Kite to Chez Scheme

-- Data types
-- ----------
--
-- Data types are compiled to Scheme record types
--
-- Product types are records with a field for each constructor argument, numbered _0 ... _N.
--
-- Sum types are records with numbered fields equal to the maximum number of args of their
-- constructor. They have an additional field _tag which indicates which constructor the object
-- belongs to. This is 0 for the first constructor (in source-code order), 1 for the second etc.
--
-- For each constructor we define a function which constructs a value with the correct tag.
-- This requires that constructors are unambiguous, so you can't have two constructors with the same
-- name in the same module. We can relax this if we compile constructor call sites directly into
-- applications of (make-<type> ...) instead.
--
-- Records are compiled to hashtables. Since they don't have constructors, we don't need to give
-- them specific constructors in scheme (I don't think).

-- Examples:
--
-- type Pair a b = Pair a b
-- (define-record-type $Pair (fields _0 _1))
--
-- type List a = Nil | Cons a (List a)
-- (define-record-type $List (fields _tag _0 _1))
--
-- type Functor f = Functor { map : forall a b. (a -> b) -> f a -> f b }
-- (define record-type $Functor (fields _0))

-- Case expressions
-- ----------------
--
-- A single-scrutinee case expression is compiled to a cond expression which tests the _tag field of
-- the type. Each branch of the cond starts with a let expression to bind the captured fields of the
-- constructor.
--
-- Example:
--
-- case (l : List Int) of
--   Nil       -> 0
--   Cons x xs -> x
--
-- (cond ((eq? 0 (List-_tag l)) 0)
--       ((eq? 1 (List-_tag l)) (let ((x (List-_0 l))
--                                    (xs (List-_1 l)))
--                                    x)))
--
-- Multi-scrutinee case expressions (i.e. mcase) are more difficult to compile efficiently, because
-- we want to minimise the number of tests we do. The pattern match compiler from ELC shows how to
-- do this, so we should be able to port that logic here.
--
freshName :: NameGen Text
freshName = freshM (\i -> ("$" <> pack (show i)))

type Env = [Def]

compileModule :: Env -> T.Module -> Env
compileModule defs m =
  let ordering :: T.Decl -> Int
      ordering = \case
        (T.DataDecl _) -> 0
        (T.FunDecl  _) -> 1
  in  defs <> concatMap compileDecl (sortOn ordering (T.moduleDecls m))

compileDecl :: T.Decl -> [Def]
compileDecl = \case
  T.FunDecl fun ->
    let (name, expr) = NameGen.run (compileFun fun) in [Def name expr]
  T.DataDecl d -> compileData d

-- | Compile a function into a pair of binding name and expression
compileFun :: T.Fun -> NameGen (Text, SExpr)
compileFun fun = do
  expr   <- compileExpr $ T.funExpr fun
  wheres <- mapM compileFun $ T.funWheres fun
  pure $ (name2Text (T.funName fun), Let wheres expr)


compileData :: T.Data -> [Def]
compileData dat =
      -- The maximum number of fields for a constructor of this type
  let
    maxFields        = maximum $ map (length . T.conArgs) (T.dataCons dat)
    fields           = "_tag" : map (pack . ('_' :) . show) [0 .. maxFields - 1]
    typeName         = "$" <> name2Text (T.dataName dat)
    recordDefinition = DefRecord typeName fields
    -- Each data type constructor is compiled to a function which constructs an object of that type with
    -- the correct _tag.
    mkConstructor :: Int -> T.DataCon -> Def
    mkConstructor tag con =
      let
        maxArgs     = length $ T.conArgs con
        parameters  = map (pack . ('_' :) . show) [0 .. maxArgs - 1]
        fieldValues = Lit (Int tag) : map Var parameters <> take
          (maxFields - maxArgs)
          (repeat null)
        body = case parameters of
          [] -> App (Var ("make-" <> typeName)) fieldValues
          _  -> Abs parameters $ App (Var ("make-" <> typeName)) fieldValues
      in
        Def (name2Text (T.conName con)) body
  in
    recordDefinition : zipWith mkConstructor [0 ..] (T.dataCons dat)

compileExpr :: T.Exp -> NameGen SExpr
compileExpr = \case
  T.IntLitT  n        -> pure $ Lit (Int n)
  T.BoolLitT b        -> pure $ Lit (Bool b)
  T.CharLitT c        -> pure $ Lit (Char c)
  T.UnitLitT          -> pure $ Lit Unit
  T.StringLitT s      -> pure $ Lit (String (pack s))
  T.TupleLitT elems _ -> App (Var "vector") <$> mapM compileExpr elems
  T.ListLitT  elems _ -> App (Var "list") <$> mapM compileExpr elems
  T.AnnT      e     _ -> compileExpr e
  T.VarT      v     _ -> pure $ Var $ name2Text v
  T.ConT c _ _        -> pure $ Var $ name2Text c
  T.AbsT vars body _ ->
    foldr (Abs . (: []) . name2Text . fst) <$> compileExpr body <*> pure vars
  T.AppT f arg _ -> do
    argExpr <- compileExpr arg
    fExpr   <- compileExpr f
    pure $ App fExpr [argExpr]
  T.LetT bindings body _ ->
    Let
      <$> mapM (\(n, e, _) -> (name2Text n, ) <$> compileExpr e) bindings
      <*> (compileExpr body)
  T.CaseT scrut alts _ -> do
    scrutVar <- freshName
    let
      -- Each case alt is compiled to a test ((eq? <tag> (<type>-_tag <scrut>)) <rhs>)
        compileAlt :: T.Pattern -> T.Exp -> NameGen SExpr
        compileAlt pat rhs = do
          let (tests, bindings) = compilePat pat (Var scrutVar)
          rhsExpr <- compileExpr rhs
          pure $ List [App (Var "and") tests, Let bindings rhsExpr]
    scrutExpr <- compileExpr scrut
    -- TODO: Use the Cond constructor
    Let [(scrutVar, scrutExpr)]
      <$> App (Var "cond")
      <$> mapM (uncurry compileAlt) alts
  -- An mcase takes N arguments and matches each against a pattern simultaneously.
  -- We compile it to a lambda that takes N arguments and then tests them in a cond, similar to
  -- case.
  T.MCaseT [] _ ->
    error $ "Chez.Compile.compileExpr: Cannot compile empty mcase"
  T.MCaseT alts@((ps, _) : _) _ -> do
    let argNum = length ps
    vars <- replicateM argNum freshName
    let compileAlt :: [T.Pattern] -> T.Exp -> NameGen SExpr
        compileAlt pats rhs = do
          rhsExpr <- compileExpr rhs
          let (tests, bindings) = compileMCaseBranch (map Var vars) pats
          pure $ List [App (Var "and") tests, Let bindings rhsExpr]
    flip (foldr (Abs . (: []))) vars
      <$> App (Var "cond")
      <$> mapM (uncurry compileAlt) alts
  T.RecordT kvs _ -> do
    r       <- freshName
    assigns <- mapM (bimapM (pure . pack) compileExpr) kvs
    pure
      $  Let [(r, App (Var "make-hashtable") [Var "symbol-hash", Var "eq?"])]
      $  begin
      $  (map
           (\(k, v) ->
             App (Var "symbol-hashtable-set!") [Var r, Quote (Var k), v]
           )
           assigns
         )
      <> [Var r]
  T.ProjectT r k _ -> do
    rExpr <- compileExpr r
    pure $ App (Var "symbol-hashtable-ref") [rExpr, Quote (Var (pack k)), false]
  T.StringInterpT prefix comps -> do
    args <- mconcatMapM
      (\(e, s) -> compileExpr e >>= \e' -> pure [e', Lit (String (pack s))])
      comps
    pure $ App (Var "string-append") (Lit (String (pack prefix)) : args)
  T.FCallT f args _ -> do
    argExprs <- mapM compileExpr args
    let fExpr = compileFCall f
    case argExprs of
      [] -> pure fExpr
      _  -> pure $ App fExpr argExprs

  -- TODO:
  -- - Holes

  e -> error $ "Cannot compile " <> show e <> "yet"

compileMCaseBranch :: [SExpr] -> [T.Pattern] -> ([SExpr], [(Text, SExpr)])
compileMCaseBranch _ [] = ([], [])
compileMCaseBranch (v : vars) (p : pats) =
  let (tests     , bindings     ) = compilePat p v
      (innerTests, innerBindings) = compileMCaseBranch vars pats
  in  (tests <> innerTests, bindings <> innerBindings)
compileMCaseBranch [] _ =
  error "Chez.Compile.compileMCaseBranch: empty set of variables"

-- Given a pattern p, and a scrutinee s, 'compilePat p s' compiles the pattern, returning a list of
-- tests to determine if the pattern matches and a list of bindings to construct if it matches.
compilePat :: T.Pattern -> SExpr -> ([SExpr], [(Text, SExpr)])
compilePat pattern scrut = case pattern of
  T.VarPat x       -> ([], [(name2Text x, scrut)])
  T.WildPat        -> ([], [])
  T.UnitPat        -> ([], [])
  T.IntPat    n    -> ([App (Var "eq?") [scrut, Lit (Int n)]], [])
  T.CharPat   c    -> ([App (Var "eq?") [scrut, Lit (Char c)]], [])
  T.StringPat s    -> ([App (Var "eq?") [scrut, Lit (String (pack s))]], [])
  T.BoolPat   b    -> ([App (Var "eq?") [scrut, Lit (Bool b)]], [])
  T.TuplePat  pats -> concatUnzip $ zipWith
    (\p i -> compilePat p (App (Var "vector-ref") [scrut, Lit (Int i)]))
    pats
    [0 :: Int ..]
  T.ListPat pats ->
    let (tests, bindings) = concatUnzip $ zipWith
          (\p i -> compilePat p (App (Var "list-ref") [scrut, Lit (Int i)]))
          pats
          [0 :: Int ..]
        lengthTest = App
          (Var "eq?")
          [App (Var "length") [scrut], Lit (Int (length pats))]
    in  (lengthTest : tests, bindings)
  -- TODO: we could use record-accessor to index into the fields of the constructor
  -- https://scheme.com/tspl4/records.html#./records:h1
  T.ConsPat c Nothing _ ->
    error $ "Chez.Compile.compilePat: no metadata for constructor " <> show c
  T.ConsPat c _ [] | c == "Kite.Primitive.[]" ->
    ([App (Var "null?") [scrut]], [])
  T.ConsPat c _ [headPat, tailPat] | c == "Kite.Primitive.::" ->
    let (headTests, headBindings) =
          compilePat headPat (App (Var "car") [scrut])
        (tailTests, tailBindings) =
          compilePat tailPat (App (Var "cdr") [scrut])
        notNullTest = App (Var "not") [App (Var "null?") [scrut]]
    in  (notNullTest : headTests <> tailTests, headBindings <> tailBindings)

  T.ConsPat _c (Just meta) pats ->
    let tag      = T.conMetaTag meta
        ty       = "$" <> name2Text (T.conMetaTypeName meta)
        tagField = ty <> "-" <> "_tag"
        fieldAtIndex i = ty <> "-_" <> pack (show i)
        ctorTest = App (Var "eq?") [App (Var tagField) [scrut], Lit (Int tag)]
        (tests, bindings) = concatUnzip $ zipWith
          (\p i -> compilePat p (App (Var (fieldAtIndex i)) [scrut]))
          pats
          [0 :: Int ..]
    in  (ctorTest : tests, bindings)

name2Text :: Name -> Text
name2Text (Local (Name n)              ) = pack n
name2Text (TopLevel moduleName (Name n)) = pack $ show moduleName ++ "." ++ n

-- Foreign calls are compiled to Chez functions or expressions
-- Some of these aren't implemented
compileFCall :: String -> SExpr
compileFCall = \case
  "getLine" -> App (Var "get-line") [App (Var "current-input-port") []]
  "putStr"  -> Abs ["s"] $ begin
    [App (Var "put-string") [App (Var "current-output-port") [], Var "s"]]
  "putStrLn" -> Abs ["s"] $ begin
    [ App (Var "put-string") [App (Var "current-output-port") [], Var "s"]
    , App (Var "newline")    [App (Var "current-output-port") []]
    ]
  f -> error $ "Unknown foreign call: " <> f

-- Implementations for compiler built-in functions like showInt
builtins :: [Def]
builtins =
  let prim s = "Kite.Primitive." <> s
  in
    [ DefRecord "$Kite.Primitive.IO" ["_tag", "_0"]
    , Def
      (prim "MkIO")
      (Abs ["f"] (App (Var ("make-$Kite.Primitive.IO")) [Lit (Int 0), Var "f"]))
    , Def
      (prim "runIO")
      (Abs
        ["m"]
        (App (App (Var ("$Kite.Primitive.IO-_0")) [Var "m"])
             [Abs ["r"] (Var "r")]
        )
      )
    , Def
      (prim "appendString")
      (Abs ["x"] (Abs ["y"] (App (Var "string-append") [Var "x", Var "y"])))
    , Def (prim "$showInt") (Var "number->string")
    , Def (prim "$eqInt")
          (Abs ["x"] (Abs ["y"] (App (Var "eq?") [Var "x", Var "y"])))
    , Def (prim "*") (Abs ["x"] (Abs ["y"] (App (Var "*") [Var "x", Var "y"])))
    , Def (prim "+") (Abs ["x"] (Abs ["y"] (App (Var "+") [Var "x", Var "y"])))
    , Def (prim "::")
          (Abs ["x"] (Abs ["xs"] (App (Var "cons") [Var "x", Var "xs"])))
    , Def (prim "$eqChar")
          (Abs ["c1"] (Abs ["c2"] (App (Var "eq?") [Var "c1", Var "c2"])))
    , Def
      (prim "$consChar")
      (Abs
        ["c"]
        (Abs
          ["s"]
          (App
            (Var "string-append")
            [App (Var "list->string") [App (Var "list") [Var "c"]], Var "s"]
          )
        )
      )
    , Def (prim "$showChar")
          (Abs ["c"] (App (Var "list->string") [App (Var "list") [Var "c"]]))
    , Def (prim "$chars") (Abs ["s"] (App (Var "string->list") [Var "s"]))
    , Def
      (prim "$unconsChar")
      (Abs
        ["s"]
        (Abs
          ["def"]
          (Abs
            ["f"]
            (Let
              [ ("len", App (Var "string-length") [Var "s"])
              , ("l"  , App (Var "string->list") [Var "s"])
              ]
              (Cond
                [ (App (Var "eq?") [Var "len", Lit (Int 0)], Var "def")
                , ( true
                  , App
                    (App (Var "f") [App (Var "car") [Var "l"]])
                    [App (Var "substring") [Var "s", Lit (Int 1), Var "len"]]
                  )
                ]
              )
            )
          )
        )
      )
    , Def
      (prim "$readInt")
      (Abs
        ["s"]
        (Abs
          ["def"]
          (Abs
            ["f"]
            (Let
              [ ("result"          , App (Var "string->number") [Var "s"])
              , ("result-is-fixnum", App (Var "fixnum?") [Var "result"])
              ]
              (Cond
                [ (Var "result-is-fixnum", App (Var "f") [Var "result"])
                , (true                  , Var "def")
                ]
              )
            )
          )
        )
      )
    ]

{-# LANGUAGE ScopedTypeVariables #-}
module Chez.Compile
  ( Env
  , Error(..)
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
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           Data.List.Extra                ( concatUnzip )
import qualified Data.List.NonEmpty            as NE
import           Data.Name                      ( Name(..)
                                                , RawName(..)
                                                , prim
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GHC.Generics                   ( Generic )
import qualified NameGen
import           NameGen                        ( NameGen
                                                , freshM
                                                )
import           Prelude                 hiding ( null )
import           Prettyprinter                  ( (<+>)
                                                , Pretty
                                                , pretty
                                                )
import qualified Syn.Typed                     as T
import           Type.Type                      ( Type )
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

data Error = EmptyMCase
           | EmptyMCaseBranch
           | CannotCompileHole Name Type
           | CannotCompileImplicit Type
           | CtorMissingMetadata Name
           | UnknownFCall String
  deriving (Eq, Generic, Show)

instance Pretty Error where
  pretty = \case
    EmptyMCase       -> "Cannot compile empty mcase"
    EmptyMCaseBranch -> "Cannot compile mcase branch with no variables"
    CannotCompileHole name _ ->
      "Encountered a hole in the program: " <+> pretty name
    CannotCompileImplicit ty ->
      "Encountered an unsolved implicit in the program : " <+> pretty (show ty)
    CtorMissingMetadata c -> "No metadata for constructor: " <+> pretty c
    UnknownFCall        n -> "Unknown foreign call: " <+> pretty n

freshName :: Monad m => NameGen m Text
freshName = freshM (\i -> "$" <> pack (show i))

type Env = [Def]

compileModule :: MonadError Error m => Env -> T.Module -> m Env
compileModule defs m = do
  let ordering :: T.Decl -> Int
      ordering = \case
        (T.DataDecl _) -> 0
        (T.FunDecl  _) -> 1
  decls <- mapM compileDecl $ sortOn ordering $ T.moduleDecls m
  pure (defs <> concat decls)

compileDecl :: MonadError Error m => T.Decl -> m [Def]
compileDecl = \case
  T.FunDecl fun -> do
    (name, expr) <- NameGen.run (compileFun fun)
    pure [Def name expr]
  T.DataDecl d -> pure $ compileData d

-- | Compile a function into a pair of binding name and expression
compileFun :: MonadError Error m => T.Fun -> NameGen m (Text, SExpr)
compileFun fun = do
  expr   <- compileExpr $ T.funExpr fun
  wheres <- mapM compileFun $ T.funWheres fun
  pure (name2Text (T.funName fun), Let wheres expr)


compileData :: T.Data -> [Def]
compileData dat =
      -- The maximum number of fields for a constructor of this type
  let
    maxFields = maximum $ map (T.conMetaArity . T.conMeta) (T.dataCons dat)
    fields           = "_tag" : map (pack . ('_' :) . show) [0 .. maxFields - 1]
    typeName         = "$" <> name2Text (T.dataName dat)
    recordDefinition = DefRecord typeName fields
    -- Each data type constructor is compiled to a function which constructs an object of that type with
    -- the correct _tag.
    mkConstructor :: Int -> T.DataCon -> Def
    mkConstructor tag con =
      let
        maxArgs    = T.conMetaArity $ T.conMeta con
        parameters = map (pack . ('_' :) . show) [0 .. maxArgs - 1]
        fieldValues =
          Lit (Int tag)
            :  map Var parameters
            <> replicate (maxFields - maxArgs) null
        body = case parameters of
          [] -> App (Var ("make-" <> typeName)) fieldValues
          _  -> Abs parameters $ App (Var ("make-" <> typeName)) fieldValues
      in
        Def (name2Text (T.conName con)) body
  in
    recordDefinition : zipWith mkConstructor [0 ..] (T.dataCons dat)

compileExpr :: forall m . MonadError Error m => T.Exp -> NameGen m SExpr
compileExpr = \case
  T.IntLitT  _ n                -> pure $ Lit (Int n)
  T.BoolLitT _ b                -> pure $ Lit (Bool b)
  T.CharLitT _ c                -> pure $ Lit (Char c)
  T.UnitLitT _                  -> pure $ Lit Unit
  T.StringLitT _ s              -> pure $ Lit (String (pack s))
  T.TupleLitT  _ elems          -> App (Var "vector") <$> mapM compileExpr elems
  T.ListLitT   _ elems          -> App (Var "list") <$> mapM compileExpr elems
  T.AnnT _ e _                  -> compileExpr e
  -- kite.Kite.Prim.[] is not a valid name in scheme, so we special case it.
  T.VarT _ v | v == prim "[]"   -> pure $ App (Var "list") []
  T.VarT _ v                    -> pure $ Var $ name2Text v
  -- kite.Kite.Prim.[] is not a valid name in scheme, so we special case it.
  T.ConT _ c _ | c == prim "[]" -> pure $ App (Var "list") []
  T.ConT _ c _                  -> pure $ Var $ name2Text c
  T.AbsT _ vars body ->
    foldr (Abs . (: []) . name2Text . fst) <$> compileExpr body <*> pure vars
  -- TODO: this is probably rubbish - check it.
  T.IAbsT _ pat _ body -> do
    varName <- freshName
    Abs [varName] <$> do
      (tests, bindings) <- compilePat pat (Var varName)
      expr              <- compileExpr body
      case tests of
        [] -> pure $ Let bindings expr
        ts ->
          pure $ App (Var "cond") [List [App (Var "and") ts, Let bindings expr]]
  T.AppT _ f arg -> do
    argExpr <- compileExpr arg
    fExpr   <- compileExpr f
    pure $ App fExpr [argExpr]
  T.IAppT _ f arg -> do
    argExpr <- compileExpr arg
    fExpr   <- compileExpr f
    pure $ App fExpr [argExpr]
  T.LetT _ bindings body ->
    Let
      <$> mapM (\(n, e, _) -> (name2Text n, ) <$> compileExpr e) bindings
      <*> compileExpr body
  T.CaseT _ scrut alts -> do
    scrutVar <- freshName
    let
      -- Each case alt is compiled to a test ((eq? <tag> (<type>-_tag <scrut>)) <rhs>)
        compileAlt :: T.Pattern -> T.Exp -> NameGen m SExpr
        compileAlt pat rhs = do
          (tests, bindings) <- compilePat pat (Var scrutVar)
          rhsExpr           <- compileExpr rhs
          case tests of
            [] -> pure $ List [Lit (Bool True), Let bindings rhsExpr]
            ts -> pure $ List [App (Var "and") ts, Let bindings rhsExpr]
    scrutExpr <- compileExpr scrut
    -- TODO: Use the Cond constructor
    Let [(scrutVar, scrutExpr)]
      .   App (Var "cond")
      <$> mapM (uncurry compileAlt) alts
  -- An mcase takes N arguments and matches each against a pattern simultaneously.
  -- We compile it to a lambda that takes N arguments and then tests them in a cond, similar to
  -- case.
  T.MCaseT _ []                 -> throwError EmptyMCase
  T.MCaseT _ alts@((ps, _) : _) -> do
    let argNum = length ps
    vars <- replicateM argNum freshName
    let compileAlt :: [T.Pattern] -> T.Exp -> NameGen m SExpr
        compileAlt pats rhs = do
          rhsExpr           <- compileExpr rhs
          (tests, bindings) <- compileMCaseBranch (map Var vars) pats
          case tests of
            [] -> pure $ List [Lit (Bool True), Let bindings rhsExpr]
            ts -> pure $ List [App (Var "and") ts, Let bindings rhsExpr]
    flip (foldr (Abs . (: []))) vars
      .   App (Var "cond")
      <$> mapM (uncurry compileAlt) alts
  T.RecordT _ kvs -> do
    r       <- freshName
    assigns <- mapM (bimapM (pure . pack) compileExpr) kvs
    pure
      $  Let [(r, App (Var "make-hashtable") [Var "symbol-hash", Var "eq?"])]
      $  begin
      $  map
           (\(k, v) ->
             App (Var "symbol-hashtable-set!") [Var r, Quote (Var k), v]
           )
           assigns

      <> [Var r]
  T.ProjectT _ r k -> do
    rExpr <- compileExpr r
    pure $ App (Var "symbol-hashtable-ref") [rExpr, Quote (Var (pack k)), false]
  T.StringInterpT _ prefix comps -> do
    args <- mconcatMapM
      (\(e, s) -> compileExpr e >>= \e' -> pure [e', Lit (String (pack s))])
      (NE.toList comps)
    pure $ App (Var "string-append") (Lit (String (pack prefix)) : args)
  T.FCallT _ f args -> do
    argExprs <- mapM compileExpr args
    let fExpr = compileFCall f
    case argExprs of
      [] -> fExpr
      _  -> App <$> fExpr <*> pure argExprs

  -- TODO:
  -- - Holes
  T.HoleT     ty name         -> throwError $ CannotCompileHole name ty
  T.ImplicitT ty (T.Solved v) -> compileExpr $ T.VarT ty v
  T.ImplicitT ty T.Unsolved   -> throwError $ CannotCompileImplicit ty

compileMCaseBranch
  :: MonadError Error m
  => [SExpr]
  -> [T.Pattern]
  -> m ([SExpr], [(Text, SExpr)])
compileMCaseBranch _          []         = pure ([], [])
compileMCaseBranch (v : vars) (p : pats) = do
  (tests     , bindings     ) <- compilePat p v
  (innerTests, innerBindings) <- compileMCaseBranch vars pats
  pure (tests <> innerTests, bindings <> innerBindings)
compileMCaseBranch [] _ = throwError EmptyMCaseBranch

-- Given a pattern p, and a scrutinee s, 'compilePat p s' compiles the pattern, returning a list of
-- tests to determine if the pattern matches and a list of bindings to construct if it matches.
compilePat
  :: MonadError Error m => T.Pattern -> SExpr -> m ([SExpr], [(Text, SExpr)])
compilePat pattern scrut = case pattern of
  T.VarPat _ x  -> pure ([], [(name2Text x, scrut)])
  T.WildPat _   -> pure ([], [])
  T.UnitPat _   -> pure ([], [])
  T.IntPat  _ n -> pure ([App (Var "eq?") [scrut, Lit (Int n)]], [])
  T.CharPat _ c -> pure ([App (Var "eq?") [scrut, Lit (Char c)]], [])
  T.StringPat _ s ->
    pure ([App (Var "eq?") [scrut, Lit (String (pack s))]], [])
  T.BoolPat  _ b    -> pure ([App (Var "eq?") [scrut, Lit (Bool b)]], [])
  T.TuplePat _ pats -> concatUnzip <$> zipWithM
    (\p i -> compilePat p (App (Var "vector-ref") [scrut, Lit (Int i)]))
    pats
    [0 :: Int ..]
  T.ListPat _ pats -> do
    let lengthTest =
          App (Var "eq?") [App (Var "length") [scrut], Lit (Int (length pats))]
    (tests, bindings) <- concatUnzip <$> zipWithM
      (\p i -> compilePat p (App (Var "list-ref") [scrut, Lit (Int i)]))
      pats
      [0 :: Int ..]
    pure (lengthTest : tests, bindings)
  -- TODO: we could use record-accessor to index into the fields of the constructor
  -- https://scheme.com/tspl4/records.html#./records:h1
  T.ConsPat _ c _ [] | c == prim "[]" -> pure ([App (Var "null?") [scrut]], [])
  T.ConsPat _ c _ [headPat, tailPat] | c == prim "::" -> do
    let notNullTest = App (Var "not") [App (Var "null?") [scrut]]
    (headTests, headBindings) <- compilePat headPat (App (Var "car") [scrut])
    (tailTests, tailBindings) <- compilePat tailPat (App (Var "cdr") [scrut])
    pure (notNullTest : headTests <> tailTests, headBindings <> tailBindings)

  T.ConsPat _ c  Nothing     _    -> throwError $ CtorMissingMetadata c
  T.ConsPat _ _c (Just meta) pats -> do
    let tag      = T.conMetaTag meta
        ty       = "$" <> name2Text (T.conMetaTypeName meta)
        tagField = ty <> "-" <> "_tag"
        fieldAtIndex i = ty <> "-_" <> pack (show i)
        ctorTest = App (Var "eq?") [App (Var tagField) [scrut], Lit (Int tag)]
    (tests, bindings) <- concatUnzip <$> zipWithM
      (\p i -> compilePat p (App (Var (fieldAtIndex i)) [scrut]))
      pats
      [0 :: Int ..]
    pure (ctorTest : tests, bindings)

name2Text :: Name -> Text
name2Text (Local (Name n)              ) = pack n
name2Text (TopLevel moduleName (Name n)) = pack $ show moduleName ++ "." ++ n

-- Foreign calls are compiled to Chez functions or expressions
-- Some of these aren't implemented
compileFCall :: MonadError Error m => String -> m SExpr
compileFCall = \case
  "getLine" -> pure $ App (Var "get-line") [App (Var "current-input-port") []]
  "putStr"  -> pure $ Abs ["s"] $ begin
    [App (Var "put-string") [App (Var "current-output-port") [], Var "s"]]
  "putStrLn" -> pure $ Abs ["s"] $ begin
    [ App (Var "put-string") [App (Var "current-output-port") [], Var "s"]
    , App (Var "newline")    [App (Var "current-output-port") []]
    ]
  f -> throwError $ UnknownFCall f

-- Implementations for compiler built-in functions like showInt
builtins :: [Def]
builtins =
  let primString s = "kite.Kite.Prim." <> s
  in
    [ DefRecord "$kite.Kite.Prim.IO" ["_tag", "_0"]
    , Def
      (primString "MkIO")
      (Abs ["f"] (App (Var "make-$kite.Kite.Prim.IO") [Lit (Int 0), Var "f"]))
    , Def
      (primString "runIO")
      (Abs
        ["m"]
        (App (App (Var "$kite.Kite.Prim.IO-_0") [Var "m"]) [Abs ["r"] (Var "r")]
        )
      )
    , Def
      (primString "appendString")
      (Abs ["x"] (Abs ["y"] (App (Var "string-append") [Var "x", Var "y"])))
    , Def (primString "$showInt") (Var "number->string")
    , Def (primString "$eqInt")
          (Abs ["x"] (Abs ["y"] (App (Var "eq?") [Var "x", Var "y"])))
    , Def (primString "*")
          (Abs ["x"] (Abs ["y"] (App (Var "*") [Var "x", Var "y"])))
    , Def (primString "+")
          (Abs ["x"] (Abs ["y"] (App (Var "+") [Var "x", Var "y"])))
    , Def (primString "-")
          (Abs ["x"] (Abs ["y"] (App (Var "-") [Var "x", Var "y"])))
    , Def (primString "/")
          (Abs ["x"] (Abs ["y"] (App (Var "div") [Var "x", Var "y"])))
    , Def (primString "::")
          (Abs ["x"] (Abs ["xs"] (App (Var "cons") [Var "x", Var "xs"])))
    , Def
      (primString ".")
      (Abs ["g"]
           (Abs ["f"] (Abs ["x"] (App (Var "g") [App (Var "f") [Var "x"]])))
      )
    , Def (primString "$eqChar")
          (Abs ["c1"] (Abs ["c2"] (App (Var "eq?") [Var "c1", Var "c2"])))
    , Def
      (primString "$consChar")
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
    , Def (primString "$showChar")
          (Abs ["c"] (App (Var "list->string") [App (Var "list") [Var "c"]]))
    , Def (primString "$chars") (Abs ["s"] (App (Var "string->list") [Var "s"]))
    , Def
      (primString "$unconsChar")
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
      (primString "$readInt")
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

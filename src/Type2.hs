module Type2 where

-- A prototype new typechecker for Lam

-- Currently just used to experiment with typechecking records

import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map

import           Canonical                      ( Name(..) )
import           Constraint
import           Constraint.Expr

type TypeEnv = Map Name Type

infer :: TypeEnv -> Exp -> Either Error Type
infer env = \case
  IntLit _ -> pure TInt
  StringLit _ components -> do
    mapM_ (infer env . fst) components
    pure TString
  Var name -> case Map.lookup name env of
                Just t -> pure t
                Nothing -> Left (UnknownVariable name)
  Abs _xs _e -> undefined -- need type variables to infer this?
  Record fields -> do
    let fieldLabels = map fst fields
        fieldExprs = map snd fields
    fieldTypes <- mapM (infer env) fieldExprs
    pure $ TRecord (zip fieldLabels fieldTypes)
  Project record label -> do
    recordType <- infer env record
    case recordType of
      TRecord fieldTypes ->
        case lookup label fieldTypes of
          Nothing -> Left (RecordDoesNotHaveLabel (TRecord fieldTypes) label)
          Just ty  -> pure ty
      nonRecordType -> Left (ProjectionOfNonRecordType nonRecordType label)
  e -> error $ "Type2.infer: cannot handle expression " <> show e

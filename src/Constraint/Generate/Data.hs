module Constraint.Generate.Data
  ( translate
  , generate
  )
where

import qualified Data.Map.Strict               as Map

import           Syn                     hiding ( Scheme
                                                , fn
                                                )
import           Data.Name
import qualified Canonical                     as Can
import qualified Syn.Typed                     as T
import           Constraint.Generate.M          ( TypeEnv )
import           Constraint.FromSyn             ( tyToType )
import           Constraint                     ( Var(R)
                                                , Type(TVar)
                                                , fn
                                                )
import           Constraint.Expr                ( Scheme )

-- Generate new bindings for data declarations.
--
--           data Maybe a = Just a | Nothing
-- generates
--           Just : Forall a. a -> Maybe a
--           Nothing : Forall a. Maybe a
--
--           data User = User { name : String, age : Int }
-- generates
--           User : String -> Int -> User
generate :: T.Data -> TypeEnv
generate d =
  Map.fromList $ map (\c -> (T.conName c, T.conType c)) (T.dataCons d)

translate :: Can.Data -> T.Data
translate d =
  let tyvars = map Local (dataTyVars d)
  in  T.Data
        { T.dataName   = dataName d
        , T.dataTyVars = tyvars
        , T.dataCons   = map (translateDataCon (dataName d) tyvars) (dataCons d)
        }

translateDataCon :: Name -> [Name] -> Can.DataCon -> T.DataCon
translateDataCon typeName tyvars datacon = case datacon of
  DataCon { conName = name, conArgs = args } -> T.DataCon
    { T.conName = name
    , T.conArgs = map tyToType args
    , T.conType = mkType typeName tyvars args
    }

mkType :: Name -> [Name] -> [Can.Type] -> Scheme
mkType dataTypeName tyvars args = T.Forall (map R tyvars)
                                           (foldr (fn . tyToType) tycon args)
  where tycon = foldl T.TApp (T.TCon dataTypeName) (map (TVar . R) tyvars)

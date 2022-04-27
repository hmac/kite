-- All errors that can be raised by the compiler are represented here
module Error
  ( Error(..)
  ) where

import           GHC.Generics                   ( Generic )

import           Prettyprinter

import qualified Chez.Compile
import qualified Interpret
import qualified ModuleLoader
import qualified Package
import qualified Type
import qualified Type.Print

data Error =
    PackageError Package.Error
  | LoadError ModuleLoader.Error
  | ParseError String
  | TypeError Type.LocatedError
  | CompileError Chez.Compile.Error
  | InterpretError Interpret.Error
  deriving (Eq, Generic, Show)

instance Pretty Error where
  pretty = \case
    PackageError   err -> pretty err
    LoadError      err -> pretty err
    ParseError     err -> pretty err
    TypeError      err -> Type.Print.printLocatedError err
    CompileError   err -> pretty err
    InterpretError err -> pretty err

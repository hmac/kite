-- 'Type' and associated types
module Type.Type
  ( Type(..)
  , E(..)
  , U(..)
  , V(..)
  , Ctx
  , CtxElem(..)
  , debugCtx
  , CtorInfo
  , TypeCtx
  ) where

import           AST                            ( ConMeta(..) )
import           Data.Data                      ( Data )
import           Data.List                      ( intercalate )
import           Data.Map.Strict                ( Map )
import           Data.Name
import           Type.Reflection                ( Typeable )
import           Util                           ( Debug(..) )

-- | A mapping from constructor names to their tag, arity and type name.
type CtorInfo = Map Name ConMeta

-- | Types
data Type =
  -- Function type
    Fn Type Type
  -- Universal quantifier
  | Forall U Type
  -- Existential variable
  | EType E
  -- Universal variable
  | UType U
  -- Type constructor (saturated)
  -- TODO: not always saturated! see subtyping for TApps
  | TCon Name [Type]
  -- Record type
  -- TODO: record typing rules
  | TRecord [(String, Type)]
  -- Type application
  -- unchecked invariant: always in spine form (i.e., head is never a TApp)
  | TApp Type [Type]
  deriving (Eq, Show, Typeable, Data)

data DebugPrintCtx = Neutral | AppL | AppR | ArrL | ArrR
instance Debug Type where
  debug = debug' AppR
   where
    debug' _ (EType e) = debug e
    debug' _ (UType u) = debug u
    debug' c (Fn a b ) = case c of
      Neutral -> debug' ArrL a <+> "->" <+> debug' ArrR b
      ArrR    -> debug' Neutral (Fn a b)
      _       -> "(" <> debug' Neutral (Fn a b) <> ")"
    debug' c (Forall v t) = case c of
      Neutral -> "∀" <> debug v <> "." <+> "(" <> debug' Neutral t <> ")"
      _       -> "(" <> debug' Neutral (Forall v t) <> ")"
    debug' _ (TCon d []  ) = debug d
    debug' c (TCon d args) = case c of
      Neutral -> debug d <+> sepBy " " (map debug args)
      AppL    -> "(" <> debug' Neutral (TCon d args) <> ")"
      AppR    -> "(" <> debug' Neutral (TCon d args) <> ")"
      _       -> debug' Neutral (TCon d args)
    debug' _ (TRecord fields) = "{" <+> sepBy ", " (map go fields) <+> "}"
      where go (name, ty) = name <+> ":" <+> debug' Neutral ty
    debug' c (TApp ty args) = case c of
      Neutral -> debug' AppL ty <+> sepBy " " (map (debug' AppR) args)
      AppR    -> "(" <> debug' Neutral (TApp ty args) <> ")"
      _       -> debug' Neutral (TApp ty args)

-- | Variables
-- Universal type variable
-- Contains a name hint
data U = U Int Name
  deriving (Show, Typeable, Data)

instance Eq U where
  (U i _) == (U j _) = i == j

instance Debug U where
  debug (U n v) = debug v <> show n

-- Existential type variable
newtype E = E Int
  deriving (Eq, Ord, Show, Typeable, Data)

instance Debug E where
  debug (E e) = "e" <> show e

-- Free or bound variable
-- Guaranteed to be unique.
-- Contains a name hint for conversion back to source.
data V = Free Name
       | Bound Int -- Not currently used, but should be for lambda bindings
  deriving (Eq, Show, Typeable, Data)

instance Debug V where
  debug (Free  n) = debug n
  debug (Bound i) = "v" <> show i

-- Contexts

-- | A local context, holding top level definitions and local variables
type Ctx = [CtxElem]

-- | A mapping of in-scope types to their kinds
-- We don't yet have kinds, so we just store () instead for the moment.
type TypeCtx = Map Name ()

data CtxElem =
  -- Bound variable
    V V Type
  -- Universal type variable
  | UVar U
  -- Existential type variable
  | EVar E
  -- Solved existential type variable (to a monotype)
  | ESolved E Type
  -- Existential variable marker
  | Marker E
  deriving (Eq, Show)

instance Debug CtxElem where
  debug (V v t       ) = debug v <+> ":" <+> debug t
  debug (UVar u      ) = debug u
  debug (EVar e      ) = debug e
  debug (ESolved e ty) = debug e <+> "=" <+> debug ty
  debug (Marker e    ) = "'" <> debug e

debugCtx :: Ctx -> String
debugCtx ctx = "[" <> sepBy ", " (map debug ctx) <> "]"

(<+>) :: String -> String -> String
a <+> b = a <> " " <> b

sepBy :: String -> [String] -> String
sepBy = intercalate

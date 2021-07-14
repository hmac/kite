-- 'Type' and associated types
module Type.Type
  ( Type(..)
  , Type'(..)
  , E(..)
  , U(..)
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
import           GHC.Generics                   ( Generic )
import           Type.Reflection                ( Typeable )
import           Util                           ( Debug(..) )

-- | A mapping from constructor names to their tag, arity and type name.
type CtorInfo = Map Name ConMeta

-- | Types
-- Kite types are split into two Haskell types: 'Type' and 'Type''.
-- 'Type' represents applications and constructors. 'Type'' represents everything else.
-- This ensures that the head of a 'TApp' is never itself a 'TApp' or a 'TCon', which guarantees
-- that applications and constructors are always in spine form - i.e. all their given arguments are
-- in the list and not in a surrounding 'TApp'
-- TODO: should we use NonEmpty lists for applications?
data Type =
    -- Type application
      TApp Type' [Type]
    -- Type constructor (saturated)
    | TCon Name [Type]
    -- All other types
    | TOther Type'
  deriving (Eq, Show, Typeable, Data, Generic)

-- | Types which are not applications
data Type' =
  -- Function type
    Fn Type Type
  -- Implicit function type
  | IFn Type Type
  -- Universal quantifier
  | Forall U Type
  -- Existential variable
  | EType E
  -- Universal variable
  | UType U
  -- Record type
  -- TODO: record typing rules
  | TRecord [(String, Type)]
  deriving (Eq, Show, Typeable, Data, Generic)

data DebugPrintCtx = Neutral | AppL | AppR | ArrL | ArrR
instance Debug Type where
  debug = go' AppR
   where
    go' c (TApp ty args) = case c of
      Neutral -> go AppL ty <+> sepBy " " (map (go' AppR) args)
      AppR    -> "(" <> go' Neutral (TApp ty args) <> ")"
      _       -> go' Neutral (TApp ty args)
    go' c (TOther t   ) = go c t
    go' _ (TCon d []  ) = debug d
    go' c (TCon d args) = case c of
      Neutral -> debug d <+> sepBy " " (map debug args)
      AppL    -> "(" <> go' Neutral (TCon d args) <> ")"
      AppR    -> "(" <> go' Neutral (TCon d args) <> ")"
      _       -> go' Neutral (TCon d args)

    go :: DebugPrintCtx -> Type' -> String
    go _ (EType e   ) = debug e
    go _ (UType u   ) = debug u
    go c (Fn     a b) = debugArrow "->" c a b
    go c (IFn    a b) = debugArrow "=>" c a b
    go c (Forall v t) = case c of
      Neutral -> "∀" <> debug v <> "." <+> "(" <> go' Neutral t <> ")"
      _       -> "(" <> go Neutral (Forall v t) <> ")"
    go _ (TRecord fields) = "{" <+> sepBy ", " (map f fields) <+> "}"
      where f (name, ty) = name <+> ":" <+> go' Neutral ty

    debugArrow arr Neutral a b = go' ArrL a <+> arr <+> go' ArrR b
    debugArrow arr ArrR    a b = debugArrow arr Neutral a b
    debugArrow arr _       a b = "(" <> debugArrow arr Neutral a b <> ")"

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

-- Contexts

-- | A local context, holding top level definitions and local variables
type Ctx = [CtxElem]

-- | A mapping of in-scope types to their kinds
-- We don't yet have kinds, so we just store () instead for the moment.
type TypeCtx = Map Name ()

data CtxElem =
  -- Term variable
    V Name Type
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
debugCtx ctx = "[" <> sepBy ", " (map debug (reverse ctx)) <> "]"

(<+>) :: String -> String -> String
a <+> b = a <> " " <> b

sepBy :: String -> [String] -> String
sepBy = intercalate

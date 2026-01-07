module AST.Types.Literal
  ( Literal (..),
    IntLiteral (..),
    IntBase (..),
    FloatLiteral (..),
    StringLiteral (..),
    BoolLiteral (..),
    ArrayLiteral (..),
  )
where

import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

data IntBase
  = BaseDec
  | BaseHex
  | BaseOct
  | BaseBin
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable IntBase

data IntLiteral = IntLiteral
  { intBase :: IntBase,
    intValue :: Integer
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable IntLiteral

newtype FloatLiteral = FloatLiteral
  { floatValue :: Double
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Hashable)

newtype StringLiteral = StringLiteral
  { -- | UTF-32 encoded string content
    stringValue :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Hashable)

newtype BoolLiteral = BoolLiteral
  { boolValue :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Hashable)

newtype ArrayLiteral expr = ArrayLiteral
  { arrayElements :: [expr]
  }
  deriving stock (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

instance (Hashable expr) => Hashable (ArrayLiteral expr)

data Literal expr
  = -- | Integer: @42@, @0xFF@
    LitInt IntLiteral
  | -- | Float: @3.14@
    LitFloat FloatLiteral
  | -- | String: @"hello world"@
    LitString StringLiteral
  | -- | Boolean: @True@, @False@
    LitBool BoolLiteral
  | -- | Array initializer: @{ 1, 2, 3 }@
    LitArray (ArrayLiteral expr)
  deriving stock (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

instance (Hashable expr) => Hashable (Literal expr)

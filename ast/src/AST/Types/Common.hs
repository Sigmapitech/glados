{-# LANGUAGE InstanceSigs #-}

module AST.Types.Common
  ( VarName (..),
    FuncName (..),
    TypeName (..),
    FieldName (..),
    ModuleName (..),
    ErrorName (..),
    FilePath' (..),
    Line (..),
    Column (..),
    Offset (..),
    SourcePos (..),
    SourceSpan (..),
    Located (..),
    unLocated,
    getSpan,
    mapLocated,
    initialPos,
    advanceCol,
    advanceLine,
    spanFromTo,
    spanSingle,
    mergeSpans,
    posToSpan,
    displayPos,
    displaySpan,
  )
where

import Data.Hashable (Hashable)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype VarName = VarName {unVarName :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, Hashable)

newtype FuncName = FuncName {unFuncName :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, Hashable)

newtype TypeName = TypeName {unTypeName :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, Hashable)

newtype FieldName = FieldName {unFieldName :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, Hashable)

newtype ModuleName = ModuleName {unModuleName :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, Hashable)

newtype ErrorName = ErrorName {unErrorName :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, Hashable)

newtype FilePath' = FilePath' {unFilePath :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, Hashable)

newtype Line = Line {unLine :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Enum, Hashable)

newtype Column = Column {unColumn :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Enum, Hashable)

newtype Offset = Offset {unOffset :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Enum, Hashable)

data SourcePos = SourcePos
  { -- | The source file path
    posFile :: !FilePath',
    -- | Line number (1-indexed)
    posLine :: !Line,
    -- | Column number (1-indexed)
    posColumn :: !Column,
    -- | Byte offset from file start (0-indexed)
    posOffset :: !Offset
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable SourcePos

instance Show SourcePos where
  show = T.unpack . displayPos

data SourceSpan = SourceSpan
  { -- | Start position (inclusive)
    spanStart :: !SourcePos,
    -- | End position (exclusive)
    spanEnd :: !SourcePos
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable SourceSpan

instance Show SourceSpan where
  show = T.unpack . displaySpan

instance Semigroup SourceSpan where
  (<>) :: SourceSpan -> SourceSpan -> SourceSpan
  s1 <> s2 =
    SourceSpan
      { spanStart = min (spanStart s1) (spanStart s2),
        spanEnd = max (spanEnd s1) (spanEnd s2)
      }

data Located a = Located
  { locSpan :: !SourceSpan,
    locValue :: a
  }
  deriving stock (Eq, Ord, Generic, Functor, Foldable, Traversable)

instance (Hashable a) => Hashable (Located a)

instance (Show a) => Show (Located a) where
  show (Located span' val) =
    T.unpack (displaySpan span') ++ ": " ++ show val

unLocated :: Located a -> a
unLocated = locValue
{-# INLINE unLocated #-}

getSpan :: Located a -> SourceSpan
getSpan = locSpan
{-# INLINE getSpan #-}

mapLocated :: (a -> b) -> Located a -> Located b
mapLocated = fmap
{-# INLINE mapLocated #-}

initialPos :: FilePath' -> SourcePos
initialPos fp =
  SourcePos
    { posFile = fp,
      posLine = Line 1,
      posColumn = Column 1,
      posOffset = Offset 0
    }

advanceCol :: SourcePos -> SourcePos
advanceCol pos =
  pos
    { posColumn = posColumn pos + 1,
      posOffset = posOffset pos + 1
    }

advanceLine :: SourcePos -> SourcePos
advanceLine pos =
  pos
    { posLine = posLine pos + 1,
      posColumn = Column 1,
      posOffset = posOffset pos + 1
    }

spanFromTo :: SourcePos -> SourcePos -> SourceSpan
spanFromTo = SourceSpan

spanSingle :: SourcePos -> SourceSpan
spanSingle pos = SourceSpan pos (advanceCol pos)

mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans = (<>)

posToSpan :: SourcePos -> SourceSpan
posToSpan pos = SourceSpan pos pos

displayPos :: SourcePos -> Text
displayPos pos =
  T.concat
    [ unFilePath (posFile pos),
      ":",
      T.pack (show (unLine (posLine pos))),
      ":",
      T.pack (show (unColumn (posColumn pos)))
    ]

displaySpan :: SourceSpan -> Text
displaySpan span'
  | posLine (spanStart span') == posLine (spanEnd span') =
      T.concat
        [ unFilePath (posFile (spanStart span')),
          ":",
          T.pack (show (unLine (posLine (spanStart span')))),
          ":",
          T.pack (show (unColumn (posColumn (spanStart span')))),
          "-",
          T.pack (show (unColumn (posColumn (spanEnd span'))))
        ]
  | otherwise =
      T.concat
        [ displayPos (spanStart span'),
          "-",
          T.pack (show (unLine (posLine (spanEnd span')))),
          ":",
          T.pack (show (unColumn (posColumn (spanEnd span'))))
        ]

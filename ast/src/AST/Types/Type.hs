module AST.Types.Type
  ( Type (..),
    PrimitiveType (..),
    IntSize (..),
    int8,
    int16,
    int32,
    int64,
    int128,
    int256,
    defaultIntSize,
    Signedness (..),
    IntType (..),
    defaultIntType,
    intTypeMaxValue,
    intTypeMinValue,
    intTypeFitsValue,
    FloatSize (..),
    floatSizeBits,
    FloatType (..),
    defaultFloatType,
    ArrayType (..),
    FunctionType (..),
    Parameter (..),
    StructType (..),
    StructField (..),
    ErrorType (..),
    ErrorField (..),
    ErrorSet (..),
    ErrorSetMember (..),
    ResultType (..),
    Constness (..),
    QualifiedType (..),
    isNumericType,
    isIntegralType,
    isFloatingType,
    typeSize,
  )
where

import AST.Types.Common (ErrorName (..), FieldName (..), Located (..), TypeName (..), VarName (..))
import Data.Hashable (Hashable)
import Data.List (intercalate)
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype IntSize = IntSize
  { -- | Number of bits (must be > 0)
    unIntSize :: Int
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, Num)

instance Show IntSize where
  show (IntSize bits) = show bits

-- | Common integer sizes as constants.
int8, int16, int32, int64, int128, int256 :: IntSize
int8 = IntSize 8
int16 = IntSize 16
int32 = IntSize 32
int64 = IntSize 64
int128 = IntSize 128
int256 = IntSize 256

defaultIntSize :: IntSize
defaultIntSize = int32

data Signedness
  = Signed
  | Unsigned
  deriving stock (Eq, Ord, Generic, Enum, Bounded)

instance Hashable Signedness

instance Show Signedness where
  show Signed = "s"
  show Unsigned = "u"

data IntType = IntType
  { intTypeSize :: IntSize,
    intTypeSign :: Signedness
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable IntType

instance Show IntType where
  show (IntType size sign)
    | size == defaultIntSize && sign == Signed = "int"
    | sign == Signed = "int<" ++ show size ++ ">"
    | otherwise = "int<" ++ show size ++ ", " ++ show sign ++ ">"

defaultIntType :: IntType
defaultIntType = IntType defaultIntSize Signed

intTypeMaxValue :: IntType -> Integer
intTypeMaxValue (IntType (IntSize bits) Signed) = 2 ^ (bits - 1) - 1
intTypeMaxValue (IntType (IntSize bits) Unsigned) = 2 ^ bits - 1

intTypeMinValue :: IntType -> Integer
intTypeMinValue (IntType (IntSize bits) Signed) = -(2 ^ (bits - 1))
intTypeMinValue (IntType (IntSize _) Unsigned) = 0

intTypeFitsValue :: IntType -> Integer -> Bool
intTypeFitsValue it val = val >= intTypeMinValue it && val <= intTypeMaxValue it

data FloatSize
  = Float32
  | Float64
  deriving stock (Eq, Ord, Generic, Enum, Bounded)

instance Hashable FloatSize

floatSizeBits :: FloatSize -> Int
floatSizeBits Float32 = 32
floatSizeBits Float64 = 64

instance Show FloatSize where
  show = show . floatSizeBits

newtype FloatType = FloatType
  { floatTypeSize :: FloatSize
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable)

instance Show FloatType where
  show (FloatType Float64) = "float"
  show (FloatType size) = "float<" ++ show size ++ ">"

defaultFloatType :: FloatType
defaultFloatType = FloatType Float64

data PrimitiveType
  = PrimInt IntType
  | PrimFloat FloatType
  | PrimBool
  | PrimString
  | PrimNone
  deriving stock (Eq, Ord, Generic)

instance Hashable PrimitiveType

instance Show PrimitiveType where
  show (PrimInt it) = show it
  show (PrimFloat ft) = show ft
  show PrimBool = "bool"
  show PrimString = "string"
  show PrimNone = "none"

newtype ArrayType = ArrayType
  { arrayTypeElement :: QualifiedType
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable ArrayType

instance Show ArrayType where
  show (ArrayType elemType) = "[" ++ show elemType ++ "]"

data Parameter = Parameter
  { paramName :: VarName,
    paramType :: QualifiedType
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable Parameter

instance Show Parameter where
  show (Parameter (VarName name) qtype) = T.unpack name ++ ": " ++ show qtype

data FunctionType = FunctionType
  { funcParams :: [Located Parameter],
    funcReturnType :: Located QualifiedType
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable FunctionType

instance Show FunctionType where
  show (FunctionType params retType) =
    "fn(" ++ intercalate ", " (map showParamType params) ++ ") -> " ++ show retType
    where
      showParamType (Located _ (Parameter _ qt)) = show (qualType qt)

data StructField = StructField
  { fieldName :: FieldName,
    fieldType :: QualifiedType
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable StructField

instance Show StructField where
  show (StructField name qtype) = T.unpack (unFieldName name) ++ ": " ++ show qtype

data StructType = StructType
  { structName :: TypeName,
    structFields :: [StructField]
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable StructType

instance Show StructType where
  show (StructType name fields) =
    "struct "
      ++ T.unpack (unTypeName name)
      ++ " { "
      ++ intercalate ", " (map show fields)
      ++ " }"

data ErrorField = ErrorField
  { errorFieldName :: FieldName,
    errorFieldType :: Type
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable ErrorField

instance Show ErrorField where
  show (ErrorField name typ) = T.unpack (unFieldName name) ++ ": " ++ show typ

data ErrorType = ErrorType
  { errorTypeName :: ErrorName,
    -- | Optional fields (empty = no payload)
    errorTypeFields :: [ErrorField]
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable ErrorType

instance Show ErrorType where
  show (ErrorType name []) = "error " ++ T.unpack (unErrorName name)
  show (ErrorType name fields) =
    "error "
      ++ T.unpack (unErrorName name)
      ++ " { "
      ++ intercalate ", " (map show fields)
      ++ " }"

data ErrorSetMember
  = ErrorMemberSingle ErrorName
  | ErrorMemberSet ErrorName
  deriving stock (Eq, Ord, Generic)

instance Hashable ErrorSetMember

instance Show ErrorSetMember where
  show (ErrorMemberSingle name) = T.unpack (unErrorName name)
  show (ErrorMemberSet name) = T.unpack (unErrorName name)

data ErrorSet = ErrorSet
  { errorSetName :: ErrorName,
    errorSetMembers :: [ErrorSetMember]
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable ErrorSet

instance Show ErrorSet where
  show (ErrorSet name members) =
    "error "
      ++ T.unpack (unErrorName name)
      ++ " = "
      ++ intercalate " | " (map show members)

data ResultType = ResultType
  { resultSuccess :: Type,
    resultError :: ErrorName
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable ResultType

instance Show ResultType where
  show (ResultType success err) =
    show success ++ " | " ++ T.unpack (unErrorName err)

data Constness
  = Mutable
  | Const
  deriving stock (Eq, Ord, Generic, Enum, Bounded)

instance Hashable Constness

instance Show Constness where
  show Mutable = ""
  show Const = "const "

data QualifiedType = QualifiedType
  { qualConstness :: Constness,
    qualType :: Type
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable QualifiedType

instance Show QualifiedType where
  show (QualifiedType Mutable typ) = show typ
  show (QualifiedType Const typ) = "const " ++ show typ

data Type
  = -- | Primitive types: @int@, @float@, @bool@, etc.
    TypePrimitive PrimitiveType
  | -- | Array types: @[int]@
    TypeArray ArrayType
  | -- | Function types: @fn(int) -> int@
    TypeFunction FunctionType
  | -- | Reference to a struct type by name
    TypeStruct TypeName
  | -- | Result/error types: @int | Error@
    TypeResult ResultType
  | -- | Reference to a named type (for forward refs)
    TypeNamed TypeName
  deriving stock (Eq, Ord, Generic)

instance Hashable Type

instance Show Type where
  show (TypePrimitive prim) = show prim
  show (TypeArray arr) = show arr
  show (TypeFunction func) = show func
  show (TypeStruct name) = T.unpack (unTypeName name)
  show (TypeResult res) = show res
  show (TypeNamed name) = T.unpack (unTypeName name)

isNumericType :: Type -> Bool
isNumericType (TypePrimitive (PrimInt _)) = True
isNumericType (TypePrimitive (PrimFloat _)) = True
isNumericType _ = False

isIntegralType :: Type -> Bool
isIntegralType (TypePrimitive (PrimInt _)) = True
isIntegralType _ = False

isFloatingType :: Type -> Bool
isFloatingType (TypePrimitive (PrimFloat _)) = True
isFloatingType _ = False

typeSize :: Type -> Maybe Int
typeSize (TypePrimitive (PrimInt it)) = Just $ unIntSize (intTypeSize it)
typeSize (TypePrimitive (PrimFloat ft)) = Just $ floatSizeBits (floatTypeSize ft)
typeSize (TypePrimitive PrimBool) = Just 1
typeSize _ = Nothing

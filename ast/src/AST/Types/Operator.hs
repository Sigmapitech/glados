module AST.Types.Operator
  ( BinaryOp (..),
    binaryOpSymbol,
    symbolToBinaryOp,
    binaryOpPrecedence,
    binaryOpAssoc,
    UnaryOp (..),
    unaryOpSymbol,
    symbolToUnaryOp,
    unaryOpPrecedence,
    unaryOpAssoc,
    AssignOp (..),
    assignOpSymbol,
    symbolToAssignOp,
    assignOpPrecedence,
    assignOpAssoc,
    assignOpToBinaryOp,
    Associativity (..),
    isArithmeticOp,
    isComparisonOp,
    isLogicalOp,
    isBitwiseOp,
  )
where

import Data.Hashable (Hashable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

data Associativity
  = -- | Left-to-right: @a - b - c@ = @(a - b) - c@
    LeftAssoc
  | -- | Right-to-left: @a = b = c@ = @a = (b = c)@
    RightAssoc
  | -- | Non-associative: @a < b < c@ is invalid
    NonAssoc
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable Associativity

data BinaryOp
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  | OpEq
  | OpNeq
  | OpLt
  | OpLte
  | OpGt
  | OpGte
  | OpAnd
  | OpOr
  | OpBitAnd
  | OpBitOr
  | OpBitXor
  | OpShl
  | OpShr
  deriving stock (Eq, Ord, Generic, Enum, Bounded)

instance Hashable BinaryOp

instance Show BinaryOp where
  show = Text.unpack . binaryOpSymbol

binaryOpSymbol :: BinaryOp -> Text
binaryOpSymbol op = case op of
  OpAdd -> "+"
  OpSub -> "-"
  OpMul -> "*"
  OpDiv -> "/"
  OpMod -> "%"
  OpEq -> "=="
  OpNeq -> "!="
  OpLt -> "<"
  OpLte -> "<="
  OpGt -> ">"
  OpGte -> ">="
  OpAnd -> "&&"
  OpOr -> "||"
  OpBitAnd -> "&"
  OpBitOr -> "|"
  OpBitXor -> "^"
  OpShl -> "<<"
  OpShr -> ">>"

binaryOpMap :: Map Text BinaryOp
binaryOpMap =
  Map.fromList
    [ (binaryOpSymbol op, op)
      | op <- [minBound .. maxBound :: BinaryOp]
    ]

symbolToBinaryOp :: Text -> Maybe BinaryOp
symbolToBinaryOp = flip Map.lookup binaryOpMap

-- | Get the precedence level for a binary operator.
--
-- Higher numbers mean tighter binding.
--
-- Precedence levels (high to low):
-- 12: * / %
-- 11: + -
-- 10: << >>
--  9: < <= > >=
--  8: == !=
--  7: &
--  6: ^
--  5: |
--  4: &&
--  3: ||
binaryOpPrecedence :: BinaryOp -> Int
binaryOpPrecedence op = case op of
  OpMul -> 12
  OpDiv -> 12
  OpMod -> 12
  OpAdd -> 11
  OpSub -> 11
  OpShl -> 10
  OpShr -> 10
  OpLt -> 9
  OpLte -> 9
  OpGt -> 9
  OpGte -> 9
  OpEq -> 8
  OpNeq -> 8
  OpBitAnd -> 7
  OpBitXor -> 6
  OpBitOr -> 5
  OpAnd -> 4
  OpOr -> 3

binaryOpAssoc :: BinaryOp -> Associativity
binaryOpAssoc op = case op of
  -- Comparison operators are non-associative
  OpEq -> NonAssoc
  OpNeq -> NonAssoc
  OpLt -> NonAssoc
  OpLte -> NonAssoc
  OpGt -> NonAssoc
  OpGte -> NonAssoc
  -- Everything else is left-associative
  _ -> LeftAssoc

data UnaryOp
  = OpNeg
  | OpPos
  | OpNot
  | OpBitNot
  deriving stock (Eq, Ord, Generic, Enum, Bounded)

instance Hashable UnaryOp

instance Show UnaryOp where
  show = Text.unpack . unaryOpSymbol

unaryOpSymbol :: UnaryOp -> Text
unaryOpSymbol op = case op of
  OpNeg -> "-"
  OpPos -> "+"
  OpNot -> "!"
  OpBitNot -> "~"

unaryOpMap :: Map Text UnaryOp
unaryOpMap =
  Map.fromList
    [ (unaryOpSymbol op, op)
      | op <- [minBound .. maxBound :: UnaryOp]
    ]

symbolToUnaryOp :: Text -> Maybe UnaryOp
symbolToUnaryOp = flip Map.lookup unaryOpMap

unaryOpPrecedence :: UnaryOp -> Int
unaryOpPrecedence _ = 13

unaryOpAssoc :: UnaryOp -> Associativity
unaryOpAssoc _ = RightAssoc

data AssignOp
  = AssignSimple
  | AssignAdd
  | AssignSub
  | AssignMul
  | AssignDiv
  | AssignMod
  | AssignBitAnd
  | AssignBitOr
  | AssignBitXor
  | AssignShl
  | AssignShr
  deriving stock (Eq, Ord, Generic, Enum, Bounded)

instance Hashable AssignOp

instance Show AssignOp where
  show = Text.unpack . assignOpSymbol

assignOpSymbol :: AssignOp -> Text
assignOpSymbol op = case op of
  AssignSimple -> "="
  AssignAdd -> "+="
  AssignSub -> "-="
  AssignMul -> "*="
  AssignDiv -> "/="
  AssignMod -> "%="
  AssignBitAnd -> "&="
  AssignBitOr -> "|="
  AssignBitXor -> "^="
  AssignShl -> "<<="
  AssignShr -> ">>="

assignOpMap :: Map Text AssignOp
assignOpMap =
  Map.fromList
    [ (assignOpSymbol op, op)
      | op <- [minBound .. maxBound :: AssignOp]
    ]

symbolToAssignOp :: Text -> Maybe AssignOp
symbolToAssignOp = flip Map.lookup assignOpMap

assignOpPrecedence :: AssignOp -> Int
assignOpPrecedence _ = 2

assignOpAssoc :: AssignOp -> Associativity
assignOpAssoc _ = RightAssoc

assignOpToBinaryOp :: AssignOp -> Maybe BinaryOp
assignOpToBinaryOp op = case op of
  AssignSimple -> Nothing
  AssignAdd -> Just OpAdd
  AssignSub -> Just OpSub
  AssignMul -> Just OpMul
  AssignDiv -> Just OpDiv
  AssignMod -> Just OpMod
  AssignBitAnd -> Just OpBitAnd
  AssignBitOr -> Just OpBitOr
  AssignBitXor -> Just OpBitXor
  AssignShl -> Just OpShl
  AssignShr -> Just OpShr

isArithmeticOp :: BinaryOp -> Bool
isArithmeticOp op = op `elem` [OpAdd, OpSub, OpMul, OpDiv, OpMod]

isComparisonOp :: BinaryOp -> Bool
isComparisonOp op = op `elem` [OpEq, OpNeq, OpLt, OpLte, OpGt, OpGte]

isLogicalOp :: BinaryOp -> Bool
isLogicalOp op = op `elem` [OpAnd, OpOr]

isBitwiseOp :: BinaryOp -> Bool
isBitwiseOp op = op `elem` [OpBitAnd, OpBitOr, OpBitXor, OpShl, OpShr]

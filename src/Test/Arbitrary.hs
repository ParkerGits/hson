module Test.Arbitrary where

import Control.Monad (liftM2, liftM3, replicateM)
import Data.List (singleton)
import qualified Data.Text as T
import HSONValue
import Parser
import Test.QuickCheck (Arbitrary (arbitrary))
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Instances
import Text.Parsec (SourcePos, oneOf)
import Text.Parsec.Pos

instance Arbitrary TokenType where
  arbitrary =
    QC.elements
      [ TokenEqual
      , TokenEqualEqual
      , TokenBang
      , TokenBangEqual
      , TokenAndAnd
      , TokenOrOr
      , TokenGreater
      , TokenGreaterEqual
      , TokenLess
      , TokenLessEqual
      , TokenMinus
      , TokenPlus
      , TokenSlash
      , TokenStar
      , TokenLeftBrace
      , TokenLeftBracket
      , TokenLeftParen
      , TokenString
      , TokenNumber
      , TokenIdentifier
      , TokenLet
      , TokenSemicolon
      , TokenColon
      , TokenQuestion
      , TokenTrue
      , TokenFalse
      , TokenNull
      , TokenArrow
      , TokenDollar
      , TokenBangBang
      , TokenQuestionQuestion
      , TokenPipeForward
      ]

alphaFreqList :: [(Int, QC.Gen Char)]
alphaFreqList =
  [ (26, QC.choose ('a', 'z'))
  , (26, QC.choose ('A', 'Z'))
  , (1, return '_')
  ]

digitFreqList :: [(Int, QC.Gen Char)]
digitFreqList = [(10, QC.choose ('0', '9'))]

letter :: QC.Gen Char
letter = QC.frequency alphaFreqList

letterOrDigit :: QC.Gen Char
letterOrDigit = QC.frequency $ alphaFreqList ++ digitFreqList

identifierGenerator =
  T.pack
    <$> liftM2 (:) letter (QC.sized (`replicateM` letterOrDigit))

instance Arbitrary HSONValue where
  arbitrary =
    QC.oneof
      [ Array <$> arbitrary
      , Object <$> arbitrary
      , String <$> arbitrary
      , Number <$> arbitrary
      , Bool <$> QC.chooseAny
      , return Null
      ]

instance Arbitrary Token where
  arbitrary = liftM3 Token arbitrary arbitrary sourcePos
   where
    sourcePos = liftM3 newPos arbitrary arbitrary arbitrary

instance Arbitrary ArrayInitializer where
  arbitrary = liftM2 ArrayInitializer arbitrary (QC.listOf arbitrary)

instance Arbitrary ArrowFunction where
  arbitrary = liftM2 ArrowFunction (QC.listOf arbitrary) arbitrary

instance Arbitrary Binary where
  arbitrary = liftM3 Binary arbitrary arbitrary arbitrary

instance Arbitrary Call where
  arbitrary = liftM3 Call arbitrary arbitrary (QC.listOf arbitrary)

instance Arbitrary Conditional where
  arbitrary = liftM3 Conditional arbitrary arbitrary arbitrary

instance Arbitrary Dollar where
  arbitrary = Dollar <$> arbitrary

instance Arbitrary Get where
  arbitrary = liftM2 Get arbitrary arbitrary

instance Arbitrary Grouping where
  arbitrary = Grouping <$> arbitrary

instance Arbitrary Index where
  arbitrary = liftM3 Index arbitrary arbitrary arbitrary

instance Arbitrary Literal where
  arbitrary = Literal <$> arbitrary

instance Arbitrary Logical where
  arbitrary = liftM3 Logical arbitrary arbitrary arbitrary

instance Arbitrary ObjectInitializer where
  arbitrary =
    liftM2
      ObjectInitializer
      arbitrary
      (QC.listOf $ liftM2 (,) arbitrary arbitrary)

instance Arbitrary Unary where
  arbitrary = liftM2 Unary arbitrary arbitrary

instance Arbitrary Variable where
  arbitrary = Variable <$> arbitrary

instance Arbitrary Expr where
  arbitrary =
    QC.oneof
      [ ArrayInitializerExpr <$> arbitrary
      , ArrowFunctionExpr <$> arbitrary
      , BinaryExpr <$> arbitrary
      , CallExpr <$> arbitrary
      , ConditionalExpr <$> arbitrary
      , DollarExpr <$> arbitrary
      , GetExpr <$> arbitrary
      , GroupingExpr <$> arbitrary
      , IndexExpr <$> arbitrary
      , LiteralExpr <$> arbitrary
      , LogicalExpr <$> arbitrary
      , ObjectInitializerExpr <$> arbitrary
      , UnaryExpr <$> arbitrary
      , VariableExpr <$> arbitrary
      ]

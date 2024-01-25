module Test.Arbitrary where

import Control.Monad (join, liftM2, liftM3, replicateM)
import Data.List (singleton)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import HSONValue
import Parser
import Test.QuickCheck (Arbitrary (arbitrary), genericShrink)
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Instances
import Text.Parsec (SourcePos, oneOf, token)
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

tokLitValGenerator :: TokenType -> QC.Gen (Maybe HSONValue)
tokLitValGenerator TokenNumber = Just . Number <$> arbitrary
tokLitValGenerator TokenString = Just . String <$> arbitrary
tokLitValGenerator TokenIdentifier = Just . String <$> identifierGenerator
tokLitValGenerator _ = return Nothing

-- Generates a Token with an appropriate literal given a TokenType
tokGenerator :: TokenType -> QC.Gen Token
tokGenerator ttype = tokArbLitGenerator ttype (tokLitValGenerator ttype)

-- Generates a Token given an arbitrary literal and a token type
tokArbLitGenerator :: TokenType -> QC.Gen (Maybe HSONValue) -> QC.Gen Token
tokArbLitGenerator ttype = tokArbLitGenerator' (pure ttype)

-- Generates a Token given an arbitrary type and arbitrary literal
tokArbLitGenerator' ::
  QC.Gen TokenType -> QC.Gen (Maybe HSONValue) -> QC.Gen Token
tokArbLitGenerator' ttype lit = liftM3 Token ttype lit arbitrary

oneTokOf :: [TokenType] -> QC.Gen Token
oneTokOf = QC.oneof . map tokGenerator

binOpGenerator :: QC.Gen Token
binOpGenerator =
  oneTokOf
    [ TokenQuestionQuestion
    , TokenEqualEqual
    , TokenBangEqual
    , TokenGreater
    , TokenGreaterEqual
    , TokenLess
    , TokenLessEqual
    , TokenMinus
    , TokenPlus
    , TokenStar
    , TokenSlash
    ]

logiOpGenerator = oneTokOf [TokenAndAnd, TokenOrOr]

unaryOpGenerator = oneTokOf [TokenBangBang, TokenBang, TokenMinus]

litGenerator :: QC.Gen Token
litGenerator = oneTokOf [TokenNumber, TokenString, TokenNull, TokenFalse, TokenTrue]

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

instance Arbitrary SourcePos where
  arbitrary = liftM3 newPos arbitrary arbitrary arbitrary

-- Generates an arbitrary Token with an appropriate literal
instance Arbitrary Token where
  arbitrary = arbitrary >>= tokGenerator

instance Arbitrary ArrayInitializer where
  arbitrary =
    liftM2
      ArrayInitializer
      (tokGenerator TokenLeftBracket)
      (QC.listOf arbitrary)

instance Arbitrary ArrowFunction where
  arbitrary = liftM2 ArrowFunction (QC.listOf $ tokGenerator TokenIdentifier) arbitrary

instance Arbitrary Binary where
  arbitrary = liftM3 Binary arbitrary binOpGenerator arbitrary

instance Arbitrary Call where
  arbitrary = liftM3 Call arbitrary (tokGenerator TokenLeftParen) (QC.listOf arbitrary)

instance Arbitrary Conditional where
  arbitrary = liftM3 Conditional arbitrary arbitrary arbitrary

instance Arbitrary Dollar where
  arbitrary = Dollar <$> tokGenerator TokenDollar

instance Arbitrary Get where
  arbitrary = liftM2 Get arbitrary (tokGenerator TokenIdentifier)

instance Arbitrary Grouping where
  arbitrary = Grouping <$> arbitrary

instance Arbitrary Index where
  arbitrary = liftM3 Index arbitrary (tokGenerator TokenLeftBracket) arbitrary

instance Arbitrary Literal where
  arbitrary = Literal <$> litGenerator

instance Arbitrary Logical where
  arbitrary = liftM3 Logical arbitrary logiOpGenerator arbitrary

instance Arbitrary ObjectInitializer where
  arbitrary =
    liftM2
      ObjectInitializer
      (tokGenerator TokenLeftBrace)
      entriesGenerator
   where
    entriesGenerator =
      QC.listOf $
        QC.oneof
          [ liftM2 (,) (tokGenerator TokenIdentifier) arbitrary
          , liftM2 (,) (tokGenerator TokenString) (pure Nothing)
          ]

instance Arbitrary Unary where
  arbitrary = liftM2 Unary unaryOpGenerator arbitrary

instance Arbitrary Variable where
  arbitrary = Variable <$> tokGenerator TokenIdentifier

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
  shrink (ArrayInitializerExpr (ArrayInitializer _ elems)) = elems
  shrink (ArrowFunctionExpr (ArrowFunction params body)) = [body]
  shrink (BinaryExpr (Binary l _ r)) = [l, r]
  shrink (CallExpr (Call callee _ args)) = callee : args
  shrink (ConditionalExpr (Conditional cond matched unmatched)) = [cond, matched, unmatched]
  shrink (DollarExpr (Dollar _)) = []
  shrink (GetExpr (Get obj _)) = [obj]
  shrink (GroupingExpr (Grouping expr)) = [expr]
  shrink (IndexExpr (Index indexed _ index)) = [indexed, index]
  shrink (LiteralExpr (Literal _)) = []
  shrink (LogicalExpr (Logical l _ r)) = [l, r]
  shrink (ObjectInitializerExpr (ObjectInitializer _ entries)) = mapMaybe snd entries
  shrink (UnaryExpr (Unary _ r)) = [r]
  shrink (VariableExpr (Variable _)) = []

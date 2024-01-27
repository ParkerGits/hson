{-# LANGUAGE OverloadedStrings #-}

module Arbitrary where

import Control.Monad (join, liftM2, liftM3, replicateM)
import Data.List (singleton)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe)
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

safeStringGenerator = T.pack <$> QC.listOf letter

identifierGenerator =
  T.pack
    <$> liftM2 (:) letter (QC.sized (`replicateM` letterOrDigit))

tokLitValGenerator :: TokenType -> QC.Gen (Maybe HSONValue)
tokLitValGenerator TokenNumber = Just . Number <$> QC.suchThat arbitrary (> 0)
tokLitValGenerator TokenString = Just . String <$> safeStringGenerator
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

-- All groups of binary operations, from lowest to highest precedence
binOpPrecedence =
  [ [TokenEqualEqual, TokenBangEqual]
  , [TokenGreater, TokenGreaterEqual, TokenLess, TokenLessEqual]
  , [TokenMinus, TokenPlus]
  , [TokenStar, TokenSlash]
  ]

binOpGenerator :: QC.Gen Token
binOpGenerator = oneTokOf $ join binOpPrecedence

-- All groups of logical operations, from lowest to highest precedence
logiOpPrecedence = [[TokenQuestionQuestion], [TokenOrOr], [TokenAndAnd]]

logiOpGenerator :: QC.Gen Token
logiOpGenerator = oneTokOf $ join logiOpPrecedence

unaryOpGenerator :: QC.Gen Token
unaryOpGenerator = oneTokOf [TokenBangBang, TokenBang, TokenMinus]

litGenerator :: QC.Gen Token
litGenerator = oneTokOf [TokenNumber, TokenString, TokenNull, TokenFalse, TokenTrue]

primaryExprGenerator :: QC.Gen Expr
primaryExprGenerator =
  QC.oneof
    [ LiteralExpr <$> arbitrary
    , DollarExpr <$> arbitrary
    , VariableExpr <$> arbitrary
    -- , GroupingExpr <$> arbitrary
    -- , ArrayInitializerExpr <$> arbitrary
    -- , ObjectInitializerExpr <$> arbitrary
    ]

-- returns an opTok with equal or higher precedence
binEqHigherPrecedenceThan :: Token -> QC.Gen (Maybe Token)
binEqHigherPrecedenceThan = higherPrecedenceThan binOpPrecedence

-- Get the binOpTok
-- Generate an expression with associated op that is either
-- 1. a binary expression with higher precedence
-- 2. a unary expression
-- 3. a primary expression
binExprPrecedenceGenerator :: Token -> QC.Gen Expr
binExprPrecedenceGenerator binOpTok = do
  higherPrecedenceOp <- binEqHigherPrecedenceThan binOpTok
  case higherPrecedenceOp of
    Nothing -> QC.oneof higherPrecedenceExprs
    Just op -> do
      l <- binExprPrecedenceGenerator op
      r <- binExprPrecedenceGenerator op
      let binExpr = BinaryExpr Binary{binLeft = l, binOp = op, binRight = r}
       in QC.oneof $
            return binExpr : higherPrecedenceExprs
 where
  higherPrecedenceExprs = [UnaryExpr <$> arbitrary, primaryExprGenerator]

-- returns an opTok with equal or higher precedence
logiEqHigherPrecedenceThan :: Token -> QC.Gen (Maybe Token)
logiEqHigherPrecedenceThan = higherPrecedenceThan logiOpPrecedence

-- Get the logiOpTok
-- Generate an expression with associated op that is either
-- 1. a logical expression with higher precedence
-- 2. a binary expression
-- 3. a unary expression
-- 4. a primary expression
logiExprPrecedenceGenerator :: Token -> QC.Gen Expr
logiExprPrecedenceGenerator logiOpTok = do
  higherPrecedenceOp <- logiEqHigherPrecedenceThan logiOpTok
  case higherPrecedenceOp of
    Nothing -> QC.oneof higherPrecedenceExprs
    Just op -> do
      l <- logiExprPrecedenceGenerator op
      r <- logiExprPrecedenceGenerator op
      let logiExpr = LogicalExpr Logical{logiLeft = l, logiOp = op, logiRight = r}
       in QC.oneof $
            return logiExpr : higherPrecedenceExprs
 where
  higherPrecedenceExprs = [BinaryExpr <$> arbitrary, UnaryExpr <$> arbitrary, primaryExprGenerator]

higherPrecedenceThan :: [[TokenType]] -> Token -> QC.Gen (Maybe Token)
higherPrecedenceThan prec (Token ttype _ _) =
  let ops = join $ tail $ dropWhile (notElem ttype) prec
   in if null ops then return Nothing else Just <$> oneTokOf ops

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

-- Ensures that operands are of equal or higher precedence than the given operation
instance Arbitrary Binary where
  arbitrary = do
    op <- binOpGenerator
    l <- binExprPrecedenceGenerator op
    r <- binExprPrecedenceGenerator op
    return $ Binary{binLeft = l, binOp = op, binRight = r}

instance Arbitrary Call where
  arbitrary =
    liftM3
      Call
      primaryExprGenerator
      (tokGenerator TokenLeftParen)
      (QC.listOf arbitrary)

-- Ensures that the condition expression is of equal or lower precedence than nullish coalescing
instance Arbitrary Conditional where
  arbitrary = liftM3 Conditional condGenerator arbitrary arbitrary
   where
    condGenerator =
      QC.oneof
        [ LogicalExpr <$> arbitrary
        , BinaryExpr <$> arbitrary
        , UnaryExpr <$> arbitrary
        , primaryExprGenerator
        ]

instance Arbitrary Dollar where
  arbitrary = Dollar <$> tokGenerator TokenDollar

instance Arbitrary Get where
  arbitrary = liftM2 Get primaryExprGenerator (tokGenerator TokenIdentifier)

instance Arbitrary Grouping where
  arbitrary = Grouping <$> arbitrary

instance Arbitrary Index where
  arbitrary =
    liftM3
      Index
      primaryExprGenerator
      (tokGenerator TokenLeftBracket)
      arbitrary

instance Arbitrary Literal where
  arbitrary = Literal <$> litGenerator

-- Ensures that operands are of equal or higher precedence than the given operation
instance Arbitrary Logical where
  arbitrary = do
    op <- logiOpGenerator
    l <- logiExprPrecedenceGenerator op
    r <- logiExprPrecedenceGenerator op
    return $ Logical{logiLeft = l, logiOp = op, logiRight = r}

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
          , liftM2 (,) (tokGenerator TokenIdentifier) (pure Nothing)
          , liftM2 (,) (tokGenerator TokenString) (Just <$> arbitrary)
          ]

instance Arbitrary Unary where
  arbitrary = liftM2 Unary unaryOpGenerator primaryExprGenerator

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
  shrink (ArrayInitializerExpr (ArrayInitializer tok elems)) =
    exprLeaves
      ++ elems
      ++ [ArrayInitializerExpr (ArrayInitializer tok elems') | elems' <- QC.shrink elems]
  shrink (ArrowFunctionExpr (ArrowFunction params body)) =
    exprLeaves
      ++ [body]
      ++ [ArrowFunctionExpr (ArrowFunction params body') | body' <- QC.shrink body]
  shrink (BinaryExpr (Binary l tok r)) =
    exprLeaves
      ++ [l, r]
      ++ [BinaryExpr (Binary l' tok r') | (l', r') <- QC.shrink (l, r)]
  shrink (CallExpr (Call callee tok args)) =
    exprLeaves
      ++ (callee : args)
      ++ [ CallExpr (Call callee' tok args') | (callee', args') <- QC.shrink (callee, args)
         ]
  shrink (ConditionalExpr (Conditional cond matched unmatched)) =
    exprLeaves
      ++ [cond, matched, unmatched]
      ++ [ ConditionalExpr (Conditional cond' matched' unmatched')
         | (cond', matched', unmatched') <- QC.shrink (cond, matched, unmatched)
         ]
  shrink (DollarExpr (Dollar _)) = []
  shrink (GetExpr (Get obj tok)) = exprLeaves ++ [obj] ++ [GetExpr (Get obj' tok) | obj' <- QC.shrink obj]
  shrink (GroupingExpr (Grouping expr)) =
    exprLeaves
      ++ [expr]
      ++ [GroupingExpr (Grouping expr') | expr' <- QC.shrink expr]
  shrink (IndexExpr (Index indexed tok index)) =
    exprLeaves
      ++ [indexed, index]
      ++ [ IndexExpr (Index indexed' tok index')
         | (indexed', index') <- QC.shrink (indexed, index)
         ]
  shrink (LiteralExpr (Literal _)) = []
  shrink (LogicalExpr (Logical l tok r)) =
    exprLeaves
      ++ [l, r]
      ++ [ LogicalExpr (Logical l' tok r')
         | (l', r') <- QC.shrink (l, r)
         ]
  shrink (ObjectInitializerExpr (ObjectInitializer tok entries)) =
    exprLeaves
      ++ mapMaybe snd entries
      ++ [ ObjectInitializerExpr (ObjectInitializer tok entries')
         | entries' <- QC.shrink entries
         ]
  shrink (UnaryExpr (Unary tok r)) = exprLeaves ++ [r] ++ [UnaryExpr (Unary tok r') | r' <- QC.shrink r]
  shrink (VariableExpr (Variable _)) = []

exprLeaves :: [Expr]
exprLeaves =
  ( VariableExpr $
      Variable
        { varName =
            Token
              { tokenType = TokenIdentifier
              , pos = leafPos
              , literal = Just $ String "leaf"
              }
        }
  )
    : ( DollarExpr $
          Dollar
            { dollarTok = Token{tokenType = TokenDollar, pos = leafPos, literal = Nothing}
            }
      )
    : map
      (LiteralExpr . Literal)
      [ Token{tokenType = TokenString, pos = leafPos, literal = Just $ String "leaf"}
      , Token{tokenType = TokenNumber, pos = leafPos, literal = Just $ Number 0}
      , Token{tokenType = TokenFalse, pos = leafPos, literal = Nothing}
      , Token{tokenType = TokenTrue, pos = leafPos, literal = Nothing}
      , Token{tokenType = TokenNull, pos = leafPos, literal = Nothing}
      ]

leafPos :: SourcePos
leafPos = newPos "leaf" 0 0

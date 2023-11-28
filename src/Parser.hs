{-# LANGUAGE OverloadedStrings #-}
module Parser ( getInput ) where

import           Control.Arrow                 (ArrowChoice (right))
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.RWS             (MonadReader (ask))
import           Data.Maybe
import           Data.Scientific               (Scientific, fromFloatDigits)
import           Data.Text                     (Text, pack, unpack)
import qualified Data.Text.Lazy                as Text
import           Lexer
import           Text.Parsec                   (ParsecT, SourcePos, alphaNum,
                                                between, getPosition, getState,
                                                letter, oneOf, putState,
                                                runParserT, sourceLine, string,
                                                try)
import qualified Text.Parsec.Token             as P
import           Text.ParserCombinators.Parsec (chainl1, char, digit, eof,
                                                getInput, many, runParser,
                                                sepBy1, space, spaces, (<|>))


data VarStmt = VarStmt
                 { declName    :: Token
                 , initializer :: Expr
                 }
  deriving (Show)


data Expr = BinaryExpr Binary
          | CallExpr Call
          | ConditionalExpr Conditional
          | GroupingExpr Grouping
          | LiteralExpr Literal
          | LogicalExpr Logical
          | UnaryExpr Unary
          | VariableExpr Variable
  deriving (Show)

data Binary = Binary
                { binLeft  :: Expr
                , binOp    :: Token
                , binRight :: Expr
                }
  deriving (Show)

data Call = Call
              { callee :: Expr
              , paren  :: Token
              , args   :: [Expr]
              }
  deriving (Show)

data Conditional = Conditional
                     { condition :: Expr
                     , matched   :: Expr
                     , unmatched :: Expr
                     }
  deriving (Show)

newtype Grouping = Grouping { groupingExpr :: Expr }
  deriving (Show)

newtype Literal = Literal { value :: LiteralValue }
  deriving (Show)

data Logical = Logical
                 { logiLeft  :: Expr
                 , logiOp    :: Token
                 , logiRight :: Expr
                 }
  deriving (Show)
data Unary = Unary
               { unaryOp    :: Token
               , unaryRight :: Expr
               }
  deriving (Show)

newtype Variable = Variable { varName :: Token }
  deriving (Show)


program :: HSONParser ([VarStmt], Expr)
program = do
  declarations <- many varDecl
  expr <- expression
  eof
  return (declarations, expr)


varDecl :: HSONParser VarStmt
varDecl = do
  letVar
  declName <- identifier
  equal
  initializer <- expression
  semicolon
  return VarStmt {declName=declName, initializer=initializer}

expression :: HSONParser Expr
expression = ternary

ternary :: HSONParser Expr
ternary = do
  expr <- logicOr
  try (do
    question
    matched <- expression
    colon
    unmatched <- expression
    return $ ConditionalExpr Conditional {condition=expr, matched=matched, unmatched=unmatched})
    <|> return expr


logicOr :: HSONParser Expr
logicOr = do
  chainl1 equality orParser
    where
      orParser = logicalOpParser orOr

logicAnd :: HSONParser Expr
logicAnd = do
  chainl1 equality andParser
    where
      andParser = logicalOpParser andAnd

equality :: HSONParser Expr
equality = do
  chainl1 comparison (try neqParser <|> eqParser)
    where
      eqParser  = binOpParser equalEqual
      neqParser = binOpParser bangEqual

comparison :: HSONParser Expr
comparison = do
  chainl1 term (try gteParser <|> try gtParser <|> try lteParser <|> ltParser)
  where
    gtParser = binOpParser greater
    gteParser = binOpParser greaterEqual
    ltParser = binOpParser less
    lteParser = binOpParser lessEqual

term :: HSONParser Expr
term = do
  chainl1 comparison (try plusParser <|> minusParser)
    where
      minusParser  = binOpParser minus
      plusParser = binOpParser plus

factor :: HSONParser Expr
factor = do
  chainl1 unary (try divParser <|> multParser)
    where
      multParser  = binOpParser star
      divParser = binOpParser slash

unary :: HSONParser Expr
unary = do
    try unaryParser <|> call
    where
      unaryParser = do
        op <- try bangParser <|> minusParser
        op <$> unary
      bangParser = unaryOpParser bang
      minusParser = unaryOpParser minus

call :: HSONParser Expr
call = do
  callee <- primary
  parenPos <- getPosition
  args <- parens arguments
  return $ CallExpr Call {callee=callee, paren=Token {tokenType=TokenLeftParen, literal=Nothing, pos=parenPos}, args=args}

primary :: HSONParser Expr
primary = do
  try falseParser <|> try trueParser <|> try nullParser <|> try numberParser <|> try stringParser <|> try identParser <|> try groupingParser
    where
      falseParser = do
        tokenFalse
        return $ LiteralExpr Literal {value=Bool False}
      trueParser = do
        tokenTrue
        return $ LiteralExpr Literal {value=Bool True}
      nullParser = do
        tokenNull
        return $ LiteralExpr Literal {value=Null}
      numberParser = do
        n <- numberLiteral
        return $ LiteralExpr Literal {value=Number $ toScientific n}
          where
            toScientific n = case n of
              Right d -> fromFloatDigits d
              Left i  -> fromInteger i
      stringParser = do
        str <- stringLiteral
        return $ LiteralExpr Literal {value=String $ pack str}
      identParser = do
        varName <- identifier
        return $ VariableExpr Variable {varName=varName}
      groupingParser = do
        expr <- parens expression
        return $ GroupingExpr $ Grouping {groupingExpr=expr}


arguments :: HSONParser [Expr]
arguments = sepBy1 expression (char ',')

logicalOpParser :: HSONParser Token -> HSONParser (Expr -> Expr -> Expr)
logicalOpParser op = do
        logiOp <- op
        pos <- getPosition
        return (\l r -> LogicalExpr Logical {logiLeft=l, logiOp=logiOp, logiRight=r})

binOpParser :: HSONParser Token -> HSONParser (Expr -> Expr -> Expr)
binOpParser op = do
        binOp <- op
        pos <- getPosition
        return (\l r -> BinaryExpr Binary {binLeft=l, binOp=binOp, binRight=r})

unaryOpParser :: HSONParser Token -> HSONParser (Expr -> Expr)
unaryOpParser op = do
      unaryOp <- op
      pos <- getPosition
      return (\r -> UnaryExpr Unary {unaryOp=unaryOp, unaryRight=r})

{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Function
import Data.Functor
import qualified Data.Map as Map
import Data.Scientific (Scientific, fromFloatDigits)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Vector as V
import HSONValue
import Lexer
import Text.Parsec (
  ParsecT,
  SourcePos,
  alphaNum,
  between,
  getPosition,
  getState,
  letter,
  oneOf,
  putState,
  runParserT,
  sourceLine,
  string,
  try,
 )
import Text.ParserCombinators.Parsec (
  chainl1,
  char,
  digit,
  eof,
  getInput,
  many,
  runParser,
  sepBy1,
  space,
  spaces,
  (<|>),
 )

data VarStmt = VarStmt
  { declName :: Token
  , initializer :: Expr
  }
  deriving (Show)

data Expr
  = ArrayInitializerExpr ArrayInitializer
  | ArrowFunctionExpr ArrowFunction
  | BinaryExpr Binary
  | CallExpr Call
  | ConditionalExpr Conditional
  | DollarExpr Dollar
  | GroupingExpr Grouping
  | GetExpr Get
  | IndexExpr Index
  | LiteralExpr Literal
  | LogicalExpr Logical
  | ObjectInitializerExpr ObjectInitializer
  | UnaryExpr Unary
  | VariableExpr Variable
  deriving (Show)

type Program = ([VarStmt], Expr)

data ArrayInitializer = ArrayInitializer
  { bracket :: Token
  , elements :: [Expr]
  }
  deriving (Show)

data ArrowFunction = ArrowFunction
  { params :: [Token]
  , body :: Expr
  }
  deriving (Show)

data Binary = Binary
  { binLeft :: Expr
  , binOp :: Token
  , binRight :: Expr
  }
  deriving (Show)

data Call = Call
  { callee :: Expr
  , paren :: Token
  , args :: [Expr]
  }
  deriving (Show)

data Conditional = Conditional
  { condition :: Expr
  , matched :: Expr
  , unmatched :: Expr
  }
  deriving (Show)

newtype Dollar = Dollar {dollarTok :: Token}
  deriving (Show)

data Get = Get
  { object :: Expr
  , property :: Token
  }
  deriving (Show)

data Index = Index
  { -- The expression being indexed
    indexed :: Expr
  , openIndexBracket :: Token
  , -- The index
    index :: Expr
  }
  deriving (Show)

newtype Grouping = Grouping {groupingExpr :: Expr}
  deriving (Show)

newtype Literal = Literal {value :: HSONValue}
  deriving (Show)

data Logical = Logical
  { logiLeft :: Expr
  , logiOp :: Token
  , logiRight :: Expr
  }
  deriving (Show)

data ObjectInitializer = ObjectInitializer
  { brace :: Token
  , entries :: [(Token, Expr)]
  }
  deriving (Show)

data Unary = Unary
  { unaryOp :: Token
  , unaryRight :: Expr
  }
  deriving (Show)

newtype Variable = Variable {varName :: Token}
  deriving (Show)

program :: HSONParser Program
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
  return VarStmt{declName = declName, initializer = initializer}

expression :: HSONParser Expr
expression = ternary

ternary :: HSONParser Expr
ternary = do
  expr <- logicOr
  try
    ( do
        question
        matched <- expression
        colon
        unmatched <- expression
        return $
          ConditionalExpr
            Conditional{condition = expr, matched = matched, unmatched = unmatched}
    )
    <|> return expr

logicOr :: HSONParser Expr
logicOr = do
  chainl1 logicAnd parseOr
 where
  parseOr = parseLogicalOp orOr

logicAnd :: HSONParser Expr
logicAnd = do
  chainl1 equality parseAnd
 where
  parseAnd = parseLogicalOp andAnd

equality :: HSONParser Expr
equality = do
  chainl1 comparison (try parseNeq <|> parseEq)
 where
  parseEq = parseBinaryOp equalEqual
  parseNeq = parseBinaryOp bangEqual

comparison :: HSONParser Expr
comparison = do
  chainl1 term (try parseGte <|> try parseGt <|> try parseLte <|> parseLt)
 where
  parseGt = parseBinaryOp greater
  parseGte = parseBinaryOp greaterEqual
  parseLt = parseBinaryOp less
  parseLte = parseBinaryOp lessEqual

term :: HSONParser Expr
term = do
  chainl1 factor (try parsePlus <|> parseMinus)
 where
  parseMinus = parseBinaryOp minus
  parsePlus = parseBinaryOp plus

factor :: HSONParser Expr
factor = do
  chainl1 unary (try parseDiv <|> parseMult)
 where
  parseMult = parseBinaryOp star
  parseDiv = parseBinaryOp slash

unary :: HSONParser Expr
unary = try parseUnary <|> call

call :: HSONParser Expr
call = do
  expr <- primary
  try (threadl expr <$> many (try $ parseCall <|> try parseIndex <|> parseGet))
    <|> return expr
 where
  -- the parsed expression becomes the callee/indexed/object for the next call/index/get expression
  -- so we apply expr to each function from left to right
  threadl = foldl (&)

primary :: HSONParser Expr
primary =
  try parseNumber
    <|> try parseString
    <|> try parseDollar
    <|> try parseTrue
    <|> try parseFalse
    <|> try parseNull
    <|> try parseIdent
    <|> try parseArray
    <|> try parseObject
    <|> try parseGrouping
    <|> parseArrowFunction

parseUnary :: HSONParser Expr
-- Parse a unary operation, then apply it to the next unary expression
parseUnary = (try parseBang <|> parseMinus) <*> unary
 where
  parseBang = parseUnaryOp bang
  parseMinus = parseUnaryOp minus

parseCall :: HSONParser (Expr -> Expr)
parseCall = do
  parenPos <- getPosition
  args <- parens arguments
  return $ \callee ->
    CallExpr
      Call
        { callee = callee
        , paren = Token{tokenType = TokenLeftParen, literal = Nothing, pos = parenPos}
        , args = args
        }

parseIndex :: HSONParser (Expr -> Expr)
parseIndex = do
  bracketPos <- getPosition
  idx <- brackets expression
  return $ \indexed ->
    IndexExpr
      Index
        { openIndexBracket =
            Token{tokenType = TokenLeftBracket, literal = Nothing, pos = bracketPos}
        , indexed = indexed
        , index = idx
        }

parseGet :: HSONParser (Expr -> Expr)
parseGet = do
  dot
  property <- identifier
  return $ \object -> GetExpr Get{object = object, property = property}

parseNumber :: HSONParser Expr
parseNumber = do
  n <- numberLiteral
  return $ LiteralExpr Literal{value = Number $ toScientific n}
 where
  toScientific n = case n of
    Right d -> fromFloatDigits d
    Left i -> fromInteger i

parseString :: HSONParser Expr
parseString = LiteralExpr . Literal . String . T.pack <$> stringLiteral

parseDollar :: HSONParser Expr
parseDollar = DollarExpr . Dollar <$> tokenDollar

parseFalse :: HSONParser Expr
parseFalse = tokenFalse $> LiteralExpr Literal{value = Bool False}

parseTrue :: HSONParser Expr
parseTrue = tokenTrue $> LiteralExpr Literal{value = Bool True}

parseNull :: HSONParser Expr
parseNull = tokenNull $> LiteralExpr Literal{value = Null}

parseIdent :: HSONParser Expr
parseIdent = VariableExpr . Variable <$> identifier

parseGrouping :: HSONParser Expr
parseGrouping = GroupingExpr . Grouping <$> parens expression

parseArrowFunction :: HSONParser Expr
parseArrowFunction = do
  tokenBackslash
  params <- parens parameters
  tokenArrow
  body <- expression
  return $ ArrowFunctionExpr ArrowFunction{params = params, body = body}

parameters :: HSONParser [Token]
parameters = commaSep identifier

arguments :: HSONParser [Expr]
arguments = commaSep expression

parseArray :: HSONParser Expr
parseArray = do
  bracketPos <- getPosition
  elems <- brackets arguments
  return $
    ArrayInitializerExpr
      ArrayInitializer
        { bracket = Token{tokenType = TokenLeftBracket, literal = Nothing, pos = bracketPos}
        , elements = elems
        }

parseObject :: HSONParser Expr
parseObject = do
  bracePos <- getPosition
  entries <- braces keyValues
  return $
    ObjectInitializerExpr
      ObjectInitializer
        { brace = Token{tokenType = TokenLeftBrace, literal = Nothing, pos = bracePos}
        , entries = entries
        }

keyValues :: HSONParser [(Token, Expr)]
keyValues = do
  commaSep keyValue
 where
  keyValue = do
    k <- identifier
    colon
    v <- expression
    return (k, v)

parseLogicalOp :: HSONParser Token -> HSONParser (Expr -> Expr -> Expr)
parseLogicalOp op = do
  logiOp <- op
  pos <- getPosition
  return (\l r -> LogicalExpr Logical{logiLeft = l, logiOp = logiOp, logiRight = r})

parseBinaryOp :: HSONParser Token -> HSONParser (Expr -> Expr -> Expr)
parseBinaryOp op = do
  binOp <- op
  pos <- getPosition
  return (\l r -> BinaryExpr Binary{binLeft = l, binOp = binOp, binRight = r})

parseUnaryOp :: HSONParser Token -> HSONParser (Expr -> Expr)
parseUnaryOp op = do
  unaryOp <- op
  pos <- getPosition
  return (\r -> UnaryExpr Unary{unaryOp = unaryOp, unaryRight = r})

parseHSON s = runIdentity $ runParserT program () "" s

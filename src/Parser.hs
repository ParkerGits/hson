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
  ParseError,
  ParsecT,
  SourcePos,
  alphaNum,
  between,
  getPosition,
  getState,
  letter,
  oneOf,
  optionMaybe,
  putState,
  runParserT,
  sourceLine,
  string,
  try,
  (<?>),
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

data VarStmt
  = VarDeclStmt VarDecl
  | ObjectDestructureDeclStmt ObjectDestructureDecl
  | ArrayDestructureDeclStmt ArrayDestructureDecl
  deriving (Show)

data VarDecl = VarDecl
  { declName :: Token
  , initializer :: Expr
  }
  deriving (Show)

data ObjectDestructureDecl = ObjectDestructureDecl
  { destKv :: [(Token, Maybe Token)]
  , destObj :: Expr
  }
  deriving (Show)

data ArrayDestructureDecl = ArrayDestructureDecl
  { destElems :: [Token]
  , destArr :: Expr
  }
  deriving (Show)

data Expr
  = ArrayInitializerExpr ArrayInitializer
  | ArrowFunctionExpr ArrowFunction
  | BinaryExpr Binary
  | CallExpr Call
  | ConditionalExpr Conditional
  | DollarExpr Dollar
  | GetExpr Get
  | GroupingExpr Grouping
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

newtype Grouping = Grouping {groupingExpr :: Expr}
  deriving (Show)

data Index = Index
  { -- The expression being indexed
    indexed :: Expr
  , openIndexBracket :: Token
  , -- The index
    index :: Expr
  }
  deriving (Show)

newtype Literal = Literal {litTok :: Token}
  deriving (Show)

data Logical = Logical
  { logiLeft :: Expr
  , logiOp :: Token
  , logiRight :: Expr
  }
  deriving (Show)

data ObjectInitializer = ObjectInitializer
  { brace :: Token
  , entries :: [(Token, Maybe Expr)]
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
  declarations <- many varStmt
  expr <- expression
  eof
  return (declarations, expr)

varStmt :: HSONParser VarStmt
varStmt = do
  letVar
  stmt <- try parseVarDecl <|> try parseObjDestDecl <|> parseArrDestDecl
  equal
  initializer <- expression
  semicolon
  return $ stmt initializer

parseVarDecl :: HSONParser (Expr -> VarStmt)
parseVarDecl = do
  declName <- identifier
  return $ \expr -> VarDeclStmt VarDecl{declName = declName, initializer = expr}

parseObjDestDecl :: HSONParser (Expr -> VarStmt)
parseObjDestDecl = do
  kv <- braces keyValues
  return $ \expr ->
    ObjectDestructureDeclStmt
      ObjectDestructureDecl{destKv = kv, destObj = expr}
 where
  keyValues = do
    commaSep (try identKeyValue <|> stringKeyValue)
   where
    identKeyValue = do
      k <- identifier
      v <- optionMaybe $ do
        colon
        identifier
      return (k, v)
    stringKeyValue = do
      k <- tokenString
      colon
      v <- identifier
      return (k, Just v)

parseArrDestDecl :: HSONParser (Expr -> VarStmt)
parseArrDestDecl = do
  elems <- brackets $ commaSep identifier
  return $ \expr ->
    ArrayDestructureDeclStmt ArrayDestructureDecl{destElems = elems, destArr = expr}

expression :: HSONParser Expr
expression = pipeForward

pipeForward :: HSONParser Expr
pipeForward = do
  expr <- ternary
  try
    ( do
        tokenPipeForward
        f <- chainl1 parsePipeInto parsePipeForward
        return $ f expr
    )
    <|> return expr
 where
  parsePipeInto = do
    into <- call
    case into of
      CallExpr f ->
        return $ \piped -> CallExpr Call{callee = callee f, paren = paren f, args = piped : args f}
      _ -> fail "can only pipe into call expressions"
  parsePipeForward = do
    tokenPipeForward
    return $ \l r piped -> r $ l piped

ternary :: HSONParser Expr
ternary = do
  expr <- nullCoalesce
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

nullCoalesce :: HSONParser Expr
nullCoalesce = do
  chainl1 logicOr parseNullCoalesce
 where
  parseNullCoalesce = parseBinaryOp questionQuestion

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
unary = try parseUnary <|> try call <?> "expression"

call :: HSONParser Expr
call = do
  expr <- primary
  try (threadl expr <$> many (try parseCall <|> try parseIndex <|> parseGet))
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
    <|> try parseArrowFunction
    <?> "expression"

parseUnary :: HSONParser Expr
-- Parse a unary operation, then apply it to the next unary expression
parseUnary = (try parseBangBang <|> try parseBang <|> parseMinus) <*> unary
 where
  parseBangBang = parseUnaryOp bangBang
  parseBang = parseUnaryOp bang
  parseMinus = parseUnaryOp minus

parseGet :: HSONParser (Expr -> Expr)
parseGet = do
  dot
  property <- identifier
  return $ \object -> GetExpr Get{object = object, property = property}

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

parseNumber :: HSONParser Expr
parseNumber = LiteralExpr . Literal <$> tokenNumber
 where
  toScientific n = case n of
    Right d -> fromFloatDigits d
    Left i -> fromInteger i

parseString :: HSONParser Expr
parseString = LiteralExpr . Literal <$> tokenString

parseDollar :: HSONParser Expr
parseDollar = DollarExpr . Dollar <$> tokenDollar

parseFalse :: HSONParser Expr
parseFalse = LiteralExpr . Literal <$> tokenFalse

parseTrue :: HSONParser Expr
parseTrue = LiteralExpr . Literal <$> tokenTrue

parseNull :: HSONParser Expr
parseNull = LiteralExpr . Literal <$> tokenNull

parseIdent :: HSONParser Expr
parseIdent = VariableExpr . Variable <$> identifier

parseGrouping :: HSONParser Expr
parseGrouping = GroupingExpr . Grouping <$> parens expression

parseArrowFunction :: HSONParser Expr
parseArrowFunction = do
  params <- pipes parameters
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
        { bracket =
            Token{tokenType = TokenLeftBracket, literal = Nothing, pos = bracketPos}
        , elements = elems
        }

parseObject :: HSONParser Expr
parseObject = do
  bracePos <- getPosition
  entries <- braces entries
  return $
    ObjectInitializerExpr
      ObjectInitializer
        { brace = Token{tokenType = TokenLeftBrace, literal = Nothing, pos = bracePos}
        , entries = entries
        }
 where
  entries = do
    commaSep (try keyValue <|> key)
   where
    keyValue = do
      k <- try tokenString <|> identifier
      colon
      v <- expression
      return (k, Just v)
    key = do
      k <- identifier
      return (k, Nothing)

parseLogicalOp :: HSONParser Token -> HSONParser (Expr -> Expr -> Expr)
parseLogicalOp op = do
  logiOp <- op
  pos <- getPosition
  return
    (\l r -> LogicalExpr Logical{logiLeft = l, logiOp = logiOp, logiRight = r})

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

parseHSON :: T.Text -> Either ParseError Program
parseHSON s = runIdentity $ runParserT program () "" s

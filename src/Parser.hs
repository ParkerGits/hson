{-# LANGUAGE OverloadedStrings #-}
module Parser ( getInput ) where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.RWS             (MonadReader (ask))
import           Data.Scientific               (Scientific)
import           Data.Text                     (Text, pack, unpack)
import qualified Data.Text.Lazy                as Text
import           GHC.IO.SubSystem              (conditional)
import           Text.Parsec                   (ParsecT, SourcePos, getPosition,
                                                getState, putState, runParserT,
                                                sourceLine, string, try)
import           Text.Parsec.Combinator        (chainl1)
import           Text.ParserCombinators.Parsec (char, digit, eof, getInput,
                                                many, runParser, sepBy1, space,
                                                spaces, (<|>))

type HSONParser = ParsecT Text () Identity

data VarStmt = VarStmt
                 { declName    :: Token
                 , initializer :: Expr
                 }
  deriving (Show)

data LiteralValue = String Text
                  | Number Scientific
  deriving (Show)

data TokenType = EqualEqual | BangEqual | AndAnd | OrOr | Greater | GreaterEqual | Less | LessEqual | Minus | Plus
  deriving (Show)

data Token = Token
               { tokenType :: TokenType
               , literal   :: Maybe LiteralValue
               , pos       :: SourcePos
               }
  deriving (Show)

data Expr = BinaryExpr Binary
          | CallExpr Call
          | ConditionalExpr Conditional
          | GroupExpr Group
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

newtype Group = Group { groupedExpr :: Expr }
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
  string "let"
  spaces
  declName <- ident
  spaces
  char '='
  spaces
  initializer <- expression
  spaces
  char ';'
  return VarStmt {declName=declName, initializer=initializer}

expression :: HSONParser Expr
expression = ternary

ternary :: HSONParser Expr
ternary = (do
  condition <- logicOr
  spaces
  char '?'
  spaces
  matched <- expression
  spaces
  char ':'
  spaces
  unmatched <- expression
  return $ ConditionalExpr Conditional {condition=condition, matched=matched, unmatched=unmatched}) <|> logicOr


logicOr :: HSONParser Expr
logicOr = do
  exprs <- sepBy1 logicAnd (string "||")
  pos <- getPosition
  return $ foldl1 (\a b -> LogicalExpr Logical {logiLeft=a, logiOp=Token {tokenType=OrOr, pos=pos, literal=Nothing}, logiRight=b}) exprs


logicAnd :: HSONParser Expr
logicAnd = do
  exprs <- sepBy1 equality (string "&&")
  pos <- getPosition
  return $ foldl1 (\a b -> LogicalExpr Logical {logiLeft=a, logiOp=Token {tokenType=AndAnd, pos=pos, literal=Nothing}, logiRight=b}) exprs

equality :: HSONParser Expr
equality = do
  chainl1 comparison (try neqParser <|> eqParser)
    where
      eqParser  = parseBinOp "==" EqualEqual
      neqParser = parseBinOp "!=" BangEqual

comparison :: HSONParser Expr
comparison = do
  chainl1 term (try gtParser <|> try gteParser <|> try ltParser <|> lteParser)
  where
    gtParser = parseBinOp ">" Greater
    gteParser = parseBinOp ">=" GreaterEqual
    ltParser = parseBinOp "<" Less
    lteParser = parseBinOp "<=" LessEqual

term :: HSONParser Expr
term = do
  chainl1 comparison (try plusParser <|> minusParser)
    where
      minusParser  = parseBinOp "-" Minus
      plusParser = parseBinOp "+" Plus

factor :: HSONParser Expr
factor = do
  chainl1 comparison (try neqParser <|> eqParser)
    where
      eqParser  = parseBinOp "==" EqualEqual
      neqParser = parseBinOp "!=" BangEqual

parseBinOp :: String -> TokenType -> HSONParser (Expr -> Expr -> Expr)
parseBinOp s tt = do
        string s
        pos <- getPosition
        return (\a b -> BinaryExpr Binary {binLeft=a, binOp=Token {tokenType=tt, literal=Nothing, pos=pos}, binRight=b})

ident :: HSONParser Token
ident = undefined

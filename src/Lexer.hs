{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (MonadReader)
import qualified Data.Map as Map
import Data.Scientific (Scientific, fromFloatDigits)
import qualified Data.Text as T (Text, pack, unpack)
import HSONValue
import Text.Parsec (
  ParsecT,
  SourcePos,
  alphaNum,
  between,
  char,
  getPosition,
  letter,
  oneOf,
  (<|>),
 )
import qualified Text.Parsec.Token as P

hsonStyle :: P.GenLanguageDef T.Text () Identity
hsonStyle =
  P.LanguageDef
    { P.reservedOpNames =
        [ "&&"
        , "||"
        , "=="
        , "!"
        , "!="
        , ">"
        , ">="
        , "<"
        , "<="
        , "-"
        , "+"
        , "/"
        , "*"
        , "?"
        , ":"
        , "=>"
        , "\\"
        , "!!"
        , "??"
        , "|>"
        ]
    , P.reservedNames = ["$", "true", "false", "let", "null"]
    , P.opStart = P.opLetter hsonStyle
    , P.opLetter = oneOf "!&*+/<=>?:\\|-"
    , P.nestedComments = True
    , P.identStart = letter <|> char '_'
    , P.identLetter = alphaNum <|> oneOf "_'"
    , P.commentStart = "/*"
    , P.commentLine = "//"
    , P.commentEnd = "*/"
    , P.caseSensitive = True
    }

hsonLexer :: P.GenTokenParser T.Text () Identity
hsonLexer = P.makeTokenParser hsonStyle

type HSONParser = ParsecT T.Text () Identity

tokenReservedOp :: TokenType -> String -> HSONParser Token
tokenReservedOp tt s = do
  pos <- getPosition
  P.reservedOp hsonLexer s
  return Token{tokenType = tt, literal = Nothing, pos = pos}

tokenReserved :: TokenType -> String -> HSONParser Token
tokenReserved tt s = do
  P.reserved hsonLexer s
  pos <- getPosition
  return Token{tokenType = tt, literal = Nothing, pos = pos}

equal = tokenReservedOp TokenEqual "="

bang = tokenReservedOp TokenBang "!"

bangBang = tokenReservedOp TokenBangBang "!!"

semicolon = tokenReservedOp TokenSemicolon ";"

colon = tokenReservedOp TokenColon ":"

question = tokenReservedOp TokenQuestion "?"

questionQuestion = tokenReservedOp TokenQuestionQuestion "??"

orOr = tokenReservedOp TokenOrOr "||"

andAnd = tokenReservedOp TokenAndAnd "&&"

equalEqual = tokenReservedOp TokenEqualEqual "=="

bangEqual = tokenReservedOp TokenBangEqual "!="

greater = tokenReservedOp TokenGreater ">"

greaterEqual = tokenReservedOp TokenGreaterEqual ">="

less = tokenReservedOp TokenLess "<"

lessEqual = tokenReservedOp TokenLessEqual "<="

minus = tokenReservedOp TokenMinus "-"

plus = tokenReservedOp TokenPlus "+"

star = tokenReservedOp TokenStar "*"

slash = tokenReservedOp TokenSlash "/"

tokenBackslash = tokenReservedOp TokenBackslash "\\"

tokenArrow = tokenReservedOp TokenArrow "=>"

tokenPipeForward = tokenReservedOp TokenPipeForward "|>"

letVar = tokenReserved TokenLet "let"

tokenTrue = tokenReserved TokenTrue "true"

tokenFalse = tokenReserved TokenFalse "false"

tokenNull = tokenReserved TokenNull "null"

tokenDollar = tokenReserved TokenDollar "$"

tokenNumber = do
  pos <- getPosition
  lit <- numberLiteral
  return
    Token
      { tokenType = TokenNumber
      , literal = Just $ Number $ toScientific lit
      , pos = pos
      }
 where
  toScientific n = case n of
    Right d -> fromFloatDigits d
    Left i -> fromInteger i

tokenString = do
  pos <- getPosition
  lit <- stringLiteral
  return
    Token
      { tokenType = TokenString
      , literal = Just $ String $ T.pack lit
      , pos = pos
      }

identifier = do
  pos <- getPosition
  name <- P.identifier hsonLexer
  return
    Token
      { tokenType = TokenIdentifier
      , literal = Just $ String $ T.pack name
      , pos = pos
      }

parens = P.parens hsonLexer

numberLiteral = P.naturalOrFloat hsonLexer

stringLiteral = P.stringLiteral hsonLexer

dot = P.dot hsonLexer

comma = P.comma hsonLexer

commaSep = P.commaSep hsonLexer

commaSep1 = P.commaSep1 hsonLexer

brackets = P.brackets hsonLexer

braces = P.braces hsonLexer

lexeme = P.lexeme hsonLexer

symbol = P.symbol hsonLexer

pipes =
  between (symbol "|") (symbol "|")

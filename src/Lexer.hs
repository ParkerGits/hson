{-# LANGUAGE OverloadedStrings #-}
module Lexer where

import           Control.Monad.Identity (Identity)
import           Data.Scientific        (Scientific)
import           Data.Text              (Text, pack)
import           Text.Parsec            (ParsecT, SourcePos, alphaNum, char,
                                         getPosition, letter, oneOf, (<|>))
import qualified Text.Parsec.Token      as P


hsonStyle :: P.GenLanguageDef Text () Identity
hsonStyle = P.LanguageDef {
  P.reservedOpNames=["&&", "||", "==", "!", "!=", ">", ">=", "<", "<=", "-", "+", "/", "*", "?", ":"], P.reservedNames=["$", "true", "false", "let", "null"], P.opStart=P.opLetter hsonStyle, P.opLetter=oneOf "!&*+/<=>?:\\|-", P.nestedComments=True, P.identStart=letter <|> char '_', P.identLetter=alphaNum <|> oneOf "_'", P.commentStart="/*", P.commentLine="//", P.commentEnd="*/", P.caseSensitive=True
  }

hsonLexer :: P.GenTokenParser Text () Identity
hsonLexer = P.makeTokenParser hsonStyle


data TokenType = TokenEqual | TokenEqualEqual | TokenBang | TokenBangEqual | TokenAndAnd | TokenOrOr | TokenGreater | TokenGreaterEqual | TokenLess | TokenLessEqual | TokenMinus | TokenPlus | TokenSlash | TokenStar | TokenLeftParen | TokenIdentifier | TokenLet | TokenSemicolon | TokenColon | TokenQuestion | TokenTrue | TokenFalse | TokenNull
  deriving (Show)

data Token = Token
               { tokenType :: TokenType
               , literal   :: Maybe LiteralValue
               , pos       :: SourcePos
               }
  deriving (Show)

data LiteralValue = String Text
                  | Number Scientific
                  | Bool Bool
                  | Null
  deriving (Show)

type HSONParser = ParsecT Text () Identity

token :: TokenType -> String -> HSONParser Token
token tt s = do
  P.symbol hsonLexer s
  pos <- getPosition
  return Token {tokenType=tt, literal=Nothing, pos=pos}

letVar = token TokenLet "let"

equal = token TokenEqual "="

bang = token TokenBang "!"

semicolon = token TokenSemicolon ";"

colon = token TokenColon ":"

question = token TokenQuestion "?"

orOr = token TokenOrOr "||"

andAnd = token TokenAndAnd "&&"

equalEqual= token TokenEqualEqual "=="

bangEqual = token TokenBangEqual "!="

greater = token TokenGreater ">"

greaterEqual = token TokenGreaterEqual ">="

less = token TokenLess "<"

lessEqual = token TokenLessEqual "<="

minus = token TokenMinus "-"

plus = token TokenPlus "+"

star = token TokenStar "*"

slash = token TokenSlash "/"

tokenTrue = token TokenTrue "true"

tokenFalse = token TokenFalse "false"

tokenNull = token TokenNull "null"

parens = P.parens hsonLexer

numberLiteral = P.naturalOrFloat hsonLexer

stringLiteral = P.stringLiteral hsonLexer

dot = P.dot hsonLexer

identifier = do
  name <- P.identifier hsonLexer
  pos <- getPosition
  return Token {tokenType=TokenIdentifier, literal=Just $ String $ pack name, pos=pos}

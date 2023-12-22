{-# LANGUAGE OverloadedStrings #-}
module Lexer where

import           Control.Monad.Identity     (Identity)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (ReaderT)
import           Control.Monad.Reader.Class (MonadReader)
import qualified Data.Map                   as Map
import           Data.Scientific            (Scientific)
import qualified Data.Text                  as T (Text, pack, unpack)
import           HSONValue
import           Text.Parsec                (ParsecT, SourcePos, alphaNum, char,
                                             getPosition, letter, oneOf, (<|>))
import qualified Text.Parsec.Token          as P


hsonStyle :: P.GenLanguageDef T.Text () Identity
hsonStyle = P.LanguageDef {
  P.reservedOpNames=["&&", "||", "==", "!", "!=", ">", ">=", "<", "<=", "-", "+", "/", "*", "?", ":"], P.reservedNames=["$", "true", "false", "let", "null"], P.opStart=P.opLetter hsonStyle, P.opLetter=oneOf "!&*+/<=>?:\\|-", P.nestedComments=True, P.identStart=letter <|> char '_', P.identLetter=alphaNum <|> oneOf "_'", P.commentStart="/*", P.commentLine="//", P.commentEnd="*/", P.caseSensitive=True
  }

hsonLexer :: P.GenTokenParser T.Text () Identity
hsonLexer = P.makeTokenParser hsonStyle


type HSONParser = ParsecT T.Text () Identity

tokenReservedOp :: TokenType -> String -> HSONParser Token
tokenReservedOp tt s = do
  P.reservedOp hsonLexer s
  pos <- getPosition
  return Token {tokenType=tt, literal=Nothing, pos=pos}

tokenReserved :: TokenType -> String -> HSONParser Token
tokenReserved tt s = do
  P.reserved hsonLexer s
  pos <- getPosition
  return Token {tokenType=tt, literal=Nothing, pos=pos}

equal = tokenReservedOp TokenEqual "="

bang = tokenReservedOp TokenBang "!"

semicolon = tokenReservedOp TokenSemicolon ";"

colon = tokenReservedOp TokenColon ":"

question = tokenReservedOp TokenQuestion "?"

orOr = tokenReservedOp TokenOrOr "||"

andAnd = tokenReservedOp TokenAndAnd "&&"

equalEqual= tokenReservedOp TokenEqualEqual "=="

bangEqual = tokenReservedOp TokenBangEqual "!="

greater = tokenReservedOp TokenGreater ">"

greaterEqual = tokenReservedOp TokenGreaterEqual ">="

less = tokenReservedOp TokenLess "<"

lessEqual = tokenReservedOp TokenLessEqual "<="

minus = tokenReservedOp TokenMinus "-"

plus = tokenReservedOp TokenPlus "+"

star = tokenReservedOp TokenStar "*"

slash = tokenReservedOp TokenSlash "/"

letVar = tokenReserved TokenLet "let"

tokenTrue = tokenReserved TokenTrue "true"

tokenFalse = tokenReserved TokenFalse "false"

tokenNull = tokenReserved TokenNull "null"

parens = P.parens hsonLexer

numberLiteral = P.naturalOrFloat hsonLexer

stringLiteral = P.stringLiteral hsonLexer

dot = P.dot hsonLexer

comma = P.comma hsonLexer

identifier = do
  name <- P.identifier hsonLexer
  pos <- getPosition
  return Token {tokenType=TokenIdentifier, literal = Just $ String $ T.pack name, pos=pos}

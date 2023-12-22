{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module HSONValue where

import           Control.Monad.Except   (ExceptT, MonadError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT)
import qualified Data.Map               as Map
import           Data.Scientific        (Scientific)
import qualified Data.Text              as T
import           Text.Parsec            (SourcePos)

data HSONValue = Function Func
               | Lambda Func Environment
               | String T.Text
               | Number Scientific
               | Bool Bool
               | Null

instance Show HSONValue where
  show = T.unpack . showValue

showValue :: HSONValue -> T.Text
showValue (Function _) = "(native function)"
showValue (Lambda _ _) = "(lambda function)"
showValue (String s)   = T.concat ["\"", s, "\""]
showValue (Number n)   = T.pack $ show n
showValue (Bool True)  = "true"
showValue (Bool False) = "false"
showValue Null         = "null"

type Environment =  Map.Map T.Text HSONValue

newtype Func = Func { fn :: [HSONValue] -> Eval HSONValue }

newtype Eval a = Eval { unEval :: ReaderT Environment (ExceptT HSONError IO) a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError HSONError
    , MonadIO
    , MonadReader Environment
    )

data HSONError = UnhandledOperator Token
               | TypeError Token String
               | UndefinedVariable Token
               | UncallableExpression Token

instance Show HSONError where
  show (UnhandledOperator (Token _ lit pos)) = case lit of
    Just (String t) -> "Unhandled binary operator \"" <> T.unpack t <> "\" at " <> show pos <> "."
    Nothing         -> "Unhandled binary operator at " <> show pos <> "."
  show (TypeError (Token _ _ pos) s)                  = "Type error at " <> show pos <> ": " <> s <> "."
  show (UndefinedVariable (Token _ (Just (String t)) pos)) = "Undefined variable \"" <> T.unpack t <> "\" at " <> show pos <> "."
  show (UncallableExpression (Token _ lit pos)) = case lit of
    Just (String t) -> "Uncallable expression \"" <> T.unpack t <> "\" at " <> show pos <> "."
    Nothing         -> "Uncallable expression at " <> show pos <> "."

data TokenType = TokenEqual | TokenEqualEqual | TokenBang | TokenBangEqual | TokenAndAnd | TokenOrOr | TokenGreater | TokenGreaterEqual | TokenLess | TokenLessEqual | TokenMinus | TokenPlus | TokenSlash | TokenStar | TokenLeftParen | TokenIdentifier | TokenLet | TokenSemicolon | TokenColon | TokenQuestion | TokenTrue | TokenFalse | TokenNull
  deriving (Show)

data Token = Token
               { tokenType :: TokenType
               , literal   :: Maybe HSONValue
               , pos       :: SourcePos
               }
  deriving (Show)

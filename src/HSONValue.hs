{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module HSONValue where

import           Control.Monad.Except   (ExceptT, MonadError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT)
import qualified Data.Map               as Map
import           Data.Scientific        (Scientific, floatingOrInteger)
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Text.Parsec            (SourcePos)

data HSONValue = Function Func
               | Method (HSONValue -> Func)
               | Lambda Func Environment
               | Array (V.Vector HSONValue)
               | Object (Map.Map T.Text HSONValue)
               | String T.Text
               | Number Scientific
               | Bool Bool
               | Null

instance Show HSONValue where
  show = T.unpack . showValue

showValue :: HSONValue -> T.Text
showValue (Function _) = "(native function)"
showValue (Method _) = "(bound function)"
showValue (Lambda _ _) = "(lambda function)"
showValue (Array v)    = T.concat ["[ ", T.intercalate ", " $ map showValue $ V.toList v, " ]"]
showValue (Object o)   = T.concat ["{ ", T.intercalate ", " $ map showEntry $ Map.toList o, " }"]
showValue (String s)   = T.concat ["\"", s, "\""]
showValue (Number n)   = case floatingOrInteger n of
  Left float -> T.pack $ show float
  Right int  -> T.pack $ show int
showValue (Bool True)  = "true"
showValue (Bool False) = "false"
showValue Null         = "null"

showEntry :: (T.Text, HSONValue) -> T.Text
showEntry (k, v) = k <> ": " <> showValue v

type Environment = Map.Map T.Text HSONValue

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
               | TypeError Token T.Text
               | UnexpectedType T.Text T.Text
               | UndefinedVariable Token
               | UncallableExpression Token
               | UndefinedProperty Token
               | InvalidIndex Token HSONValue T.Text
               | IndexOutOfBounds Token Int
               | ArgumentCount Int [HSONValue]
               | CallError Token HSONError

instance Show HSONError where
  show = T.unpack . showError

showError :: HSONError -> T.Text
showError (UnhandledOperator (Token _ lit pos)) = case lit of
  Just (String t) -> T.concat ["Unhandled binary operator \"", t, "\" at ", T.pack $ show pos, "."]
  Nothing         -> T.concat ["Unhandled binary operator at ", T.pack $ show pos, "."]
showError (TypeError (Token _ _ pos) msg)                  = T.concat["Type error at ", T.pack $ show pos, ": ", msg, "."]
showError (UnexpectedType expected received) = T.concat ["expected ", expected, " received ", received]
showError (UndefinedVariable (Token _ (Just (String t)) pos)) = T.concat ["Undefined variable \"", t, "\" at ", T.pack $ show pos, "."]
showError (UncallableExpression (Token _ lit pos)) = case lit of
  Just (String t) -> T.concat ["Uncallable expression \"", t, "\" at ", T.pack $ show pos, "."]
  Nothing         -> T.concat ["Uncallable expression at ", T.pack $ show pos, "."]
showError (UndefinedProperty (Token _ (Just (String t)) pos)) = T.concat ["Property ", t, " does not exist at ", T.pack $ show pos, "."]
showError (InvalidIndex (Token _ _ pos) val objType) = T.concat ["Cannot index ", objType, " with ", showValue val, " at ", T.pack $ show pos, "."]
showError (IndexOutOfBounds (Token _ _ pos) idx) = T.concat ["Index ", T.pack $ show idx, " out of bounds at ", T.pack $ show pos, "."]
showError (ArgumentCount expected received) = T.concat ["expected ", T.pack $ show expected, " arguments, received args [", T.intercalate ", " $ map showValue received, "]" ]
showError (CallError (Token _ _ pos) err) = T.concat ["Call error at ", T.pack $ show pos, ": ", showError err, "."]

data TokenType = TokenEqual | TokenEqualEqual | TokenBang | TokenBangEqual | TokenAndAnd | TokenOrOr | TokenGreater | TokenGreaterEqual | TokenLess | TokenLessEqual | TokenMinus | TokenPlus | TokenSlash | TokenStar | TokenLeftBrace | TokenLeftBracket | TokenLeftParen | TokenIdentifier | TokenLet | TokenSemicolon | TokenColon | TokenQuestion | TokenTrue | TokenFalse | TokenNull | TokenBackslash | TokenArrow
  deriving (Show)

data Token = Token
               { tokenType :: TokenType
               , literal   :: Maybe HSONValue
               , pos       :: SourcePos
               }
  deriving (Show)

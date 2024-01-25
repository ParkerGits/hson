{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module HSONValue where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.Map as Map
import Data.Ord (Down (Down))
import Data.Scientific (Scientific, floatingOrInteger)
import qualified Data.Text as T
import Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Text.Parsec (SourcePos)

data HSONValue
  = Function Func
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

instance Eq HSONValue where
  Array x == Array y = x == y
  String x == String y = x == y
  Number x == Number y = x == y
  Bool x == Bool y = x == y
  Null == Null = True
  (==) _ _ = False

instance Ord HSONValue where
  compare (String x) (String y) = compare x y
  compare (Number x) (Number y) = compare x y
  compare (Bool x) (Bool y) = compare x y
  compare (Object x) (Object y) = compare x y
  compare _ _ = EQ

data SortedValue
  = Asc HSONValue
  | Desc HSONValue
  | AscByKey T.Text HSONValue
  | DescByKey T.Text HSONValue
  deriving (Eq, Show)

instance Ord SortedValue where
  compare (Asc x) (Asc y) = compare x y
  compare (Desc x) (Desc y) = compare (Down x) (Down y)
  compare (AscByKey t1 (Object x)) (AscByKey t2 (Object y)) =
    case (Map.lookup t1 x, Map.lookup t2 y) of
      (Nothing, Nothing) -> EQ
      (Nothing, Just _) -> LT
      (Just _, Nothing) -> GT
      (Just v1, Just v2) -> compare v1 v2
  compare (AscByKey _ x) (AscByKey _ y) = compare (Asc x) (Asc y)
  compare (DescByKey t1 (Object x)) (DescByKey t2 (Object y)) =
    case (Map.lookup t1 x, Map.lookup t2 y) of
      (Nothing, Nothing) -> EQ
      (Nothing, Just _) -> GT
      (Just _, Nothing) -> LT
      (Just v1, Just v2) -> compare (Down v1) (Down v2)
  compare (DescByKey _ x) (DescByKey _ y) = compare (Desc x) (Desc y)

fromSorted :: SortedValue -> HSONValue
fromSorted (Asc v) = v
fromSorted (Desc v) = v
fromSorted (AscByKey _ v) = v
fromSorted (DescByKey _ v) = v

showValue :: HSONValue -> T.Text
showValue (Function _) = "(native function)"
showValue (Method _) = "(bound function)"
showValue (Lambda _ _) = "(lambda function)"
showValue (Array v) = T.concat ["[", T.intercalate ", " $ map showValue $ V.toList v, "]"]
showValue (Object o) = T.concat ["{ ", T.intercalate ", " $ map showEntry $ Map.toList o, " }"]
showValue (String s) = s
showValue (Number n) = case floatingOrInteger n of
  Left float -> T.pack $ show float
  Right int -> T.pack $ show int
showValue (Bool True) = "true"
showValue (Bool False) = "false"
showValue Null = "null"

showEntry :: (T.Text, HSONValue) -> T.Text
showEntry (k, v) = k <> ": " <> showValue v

type Environment = Map.Map T.Text HSONValue

newtype Func = Func {fn :: [HSONValue] -> Eval HSONValue} deriving ()

newtype Eval a = Eval {unEval :: ReaderT Environment (ExceptT HSONError IO) a}
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError HSONError
    , MonadIO
    , MonadReader Environment
    )

data HSONError
  = UnhandledOperator Token
  | TypeError Token T.Text
  | UnexpectedType T.Text T.Text
  | UndefinedVariable Token
  | UncallableExpression Token
  | UndefinedProperty T.Text Token
  | InvalidIndex Token HSONValue T.Text
  | IndexOutOfBounds Token Int
  | IndexOutOfBounds' Int
  | ArgumentCount Int [HSONValue]
  | VariadicArgCount Int Int [HSONValue]
  | CallError Token HSONError
  | JSONParsingError T.Text
  | JSONEncodingError UnicodeException

instance Show HSONError where
  show = T.unpack . showError

showError :: HSONError -> T.Text
showError (UnhandledOperator (Token _ lit pos)) = case lit of
  Just (String t) ->
    T.concat ["Unhandled binary operator \"", t, "\" at ", T.pack $ show pos, "."]
  Nothing -> T.concat ["Unhandled binary operator at ", T.pack $ show pos, "."]
showError (TypeError (Token _ _ pos) msg) = T.concat ["Type error at ", T.pack $ show pos, ": ", msg]
showError (UnexpectedType expected received) = T.concat ["Expected ", expected, ", received ", received]
showError (UndefinedVariable (Token _ (Just (String t)) pos)) = T.concat ["Undefined variable \"", t, "\" at ", T.pack $ show pos]
showError (UncallableExpression (Token _ lit pos)) = case lit of
  Just (String t) -> T.concat ["Uncallable expression \"", t, "\" at ", T.pack $ show pos, "."]
  Nothing -> T.concat ["Uncallable expression at ", T.pack $ show pos, "."]
showError (UndefinedProperty name (Token _ _ pos)) = T.concat ["Property ", name, " does not exist at ", T.pack $ show pos, "."]
showError (InvalidIndex (Token _ _ pos) val objType) =
  T.concat
    [ "Cannot index "
    , objType
    , " with "
    , showValue val
    , " at "
    , T.pack $ show pos
    , "."
    ]
showError (IndexOutOfBounds (Token _ _ pos) idx) =
  T.concat
    ["Index ", T.pack $ show idx, " out of bounds at ", T.pack $ show pos, "."]
showError (IndexOutOfBounds' idx) =
  T.concat
    ["Index ", T.pack $ show idx, " out of bounds"]
showError (ArgumentCount expected received) =
  T.concat
    [ "Expected "
    , T.pack $ show expected
    , " "
    , if expected /= 1 then "arguments" else "argument"
    , ", received args ["
    , T.intercalate ", " $ map showValue received
    , "]"
    ]
showError (VariadicArgCount minExpected maxExpected received) =
  T.concat
    [ "Expected "
    , T.pack $ show minExpected
    , " to "
    , T.pack $ show maxExpected
    , " arguments, received args ["
    , T.intercalate ", " $ map showValue received
    , "]"
    ]
showError (CallError (Token _ _ pos) err) = T.concat ["Call error at ", T.pack $ show pos, ": ", showError err, "."]
showError (JSONParsingError err) = T.concat ["Error parsing input JSON: ", err]

data TokenType
  = TokenEqual
  | TokenEqualEqual
  | TokenBang
  | TokenBangEqual
  | TokenAndAnd
  | TokenOrOr
  | TokenGreater
  | TokenGreaterEqual
  | TokenLess
  | TokenLessEqual
  | TokenMinus
  | TokenPlus
  | TokenSlash
  | TokenStar
  | TokenLeftBrace
  | TokenLeftBracket
  | TokenLeftParen
  | TokenString
  | TokenNumber
  | TokenIdentifier
  | TokenLet
  | TokenSemicolon
  | TokenColon
  | TokenQuestion
  | TokenTrue
  | TokenFalse
  | TokenNull
  | TokenArrow
  | TokenDollar
  | TokenBangBang
  | TokenQuestionQuestion
  | TokenPipeForward
  deriving (Show, Enum, Eq)

data Token = Token
  { tokenType :: TokenType
  , literal :: Maybe HSONValue
  , pos :: SourcePos
  }
  deriving (Show)

-- Two tokens are equals if they contain the same TokenType and Literal; pos is not checked!
instance Eq Token where
  (Token tta lita _) == (Token ttb litb _) = tta == ttb && lita == litb

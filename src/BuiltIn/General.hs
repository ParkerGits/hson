{-# LANGUAGE OverloadedStrings #-}

module BuiltIn.General where

import BuiltIn.Function (toJSON)
import BuiltIn.Helpers
import Control.Exception
import Control.Monad.Except
import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty (defConfig)
import qualified Data.Aeson.Encode.Pretty as AP
import Data.List
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Vector as V
import HSONValue
import JSONParser

hsonLength :: MethodDefinition
hsonLength this [] = case this of
  Array arr -> return $ Number $ fromIntegral $ V.length arr
  String str -> return $ Number $ fromIntegral $ T.length str
hsonLength _ args = throwError $ ArgumentCount 0 args

hsonToString :: MethodDefinition
hsonToString this [] = return $ String $ showValue this
hsonToString _ args = throwError $ ArgumentCount 0 args

hsonToJSON :: MethodDefinition
hsonToJSON this [Number n] = toJSON [this, Number n]
hsonToJSON this [] = toJSON [this]
hsonToJSON _ [arg] = throwError $ UnexpectedType "number" (showType arg)
hsonToJSON _ args = throwError $ VariadicArgCount 0 1 args

-- Returns the value at the index n. If n is negative, the checked index is index + n. If n is out of bounds, returns Null.
hsonAt :: MethodDefinition
hsonAt this [Number n] = do
  idx <- indexFromNumber n this
  case this of
    Array arr -> case arr V.!? idx of
      Just v -> return v
      Nothing -> return Null
    String str -> do
      result <- liftIO $ try $ evaluate $ T.index str idx
      case result :: Either SomeException Char of
        Right result -> return $ String $ T.singleton result
        Left _ -> return Null
hsonAt _ [arg] = throwError $ UnexpectedType "number" (showType arg)
hsonAt _ args = throwError $ ArgumentCount 1 args

hsonReverse :: MethodDefinition
hsonReverse this [] = case this of
  Array arr -> return $ Array $ V.reverse arr
  String str -> return $ String $ T.reverse str
hsonReverse _ args = throwError $ ArgumentCount 0 args

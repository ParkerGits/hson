{-# LANGUAGE OverloadedStrings #-}

module Methods.General where

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
import Methods.Helpers

hsonLength :: HSONValue -> [HSONValue] -> Eval HSONValue
hsonLength this [] = do
  case this of
    Array arr -> return $ Number $ fromIntegral $ V.length arr
    String str -> return $ Number $ fromIntegral $ T.length str
hsonLength _ args = throwError $ ArgumentCount 0 args

hsonToString :: HSONValue -> [HSONValue] -> Eval HSONValue
hsonToString this [] = return $ String $ showValue this
hsonToString _ args = throwError $ ArgumentCount 0 args

hsonToJSON :: HSONValue -> [HSONValue] -> Eval HSONValue
hsonToJSON this [] = case T.decodeUtf8' $ A.encode this of
  Left err -> throwError $ JSONEncodingError err
  Right t -> return $ String $ T.toStrict t
hsonToJSON this [Number n] = do
  spaces <- intFromNumber n
  case T.decodeUtf8' $
    AP.encodePretty'
      (encodePrettyConfig spaces)
      this of
    Left err -> throwError $ JSONEncodingError err
    Right t -> return $ String $ T.toStrict t
hsonToJSON _ [arg] = throwError $ UnexpectedType "number" (showType arg)
hsonToJSON _ args = throwError $ VariadicArgCount 0 1 args

hsonAt :: HSONValue -> [HSONValue] -> Eval HSONValue
hsonAt this [Number n] = do
  idx <- indexFromNumber n this
  case this of
    Array arr -> case arr V.!? idx of
      Just v -> return v
      Nothing -> return Null
hsonAt _ [arg] = throwError $ UnexpectedType "number" (showType arg)
hsonAt _ args = throwError $ ArgumentCount 1 args

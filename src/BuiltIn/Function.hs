{-# LANGUAGE OverloadedStrings #-}

module BuiltIn.Function where

import BuiltIn.Helpers
import Control.Monad.Error.Class
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Vector as V
import HSONValue
import JSONParser

builtInFunctions :: Map.Map T.Text HSONValue
builtInFunctions =
  Map.fromList
    [ ("keys", mkFunction keys)
    , ("values", mkFunction values)
    , ("hasProperty", mkFunction hasProperty)
    , ("toJSON", mkFunction toJSON)
    ]

keys :: FunctionDefinition
keys [Object o] = return $ Array $ V.map String $ V.fromList $ Map.keys o
keys [arg] = throwError $ UnexpectedType "object" (showType arg)
keys args = throwError $ ArgumentCount 1 args

values :: FunctionDefinition
values [Object o] = return $ Array $ V.fromList $ Map.elems o
values [arg] = throwError $ UnexpectedType "object" (showType arg)
values args = throwError $ ArgumentCount 1 args

hasProperty :: FunctionDefinition
hasProperty [Object o, String key] = return $ Bool $ Map.member key o
hasProperty [Object _, arg] = throwError $ UnexpectedType "string" (showType arg)
hasProperty [arg, _] = throwError $ UnexpectedType "object" (showType arg)
hasProperty args = throwError $ ArgumentCount 2 args

toJSON :: FunctionDefinition
toJSON [arg, Number n] = do
  spaces <- intFromNumber n
  case T.decodeUtf8' $
    AP.encodePretty'
      (encodePrettyConfig spaces)
      arg of
    Left err -> throwError $ JSONEncodingError err
    Right t -> return $ String $ T.toStrict t
toJSON [_, arg] = throwError $ UnexpectedType "number" (showType arg)
toJSON [arg] = case T.decodeUtf8' $ A.encode arg of
  Left err -> throwError $ JSONEncodingError err
  Right t -> return $ String $ T.toStrict t
toJSON args = throwError $ VariadicArgCount 1 2 args

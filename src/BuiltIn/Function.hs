{-# LANGUAGE OverloadedStrings #-}

module BuiltIn.Function where

import BuiltIn.Helpers
import Control.Exception
import Control.Monad.Cont (MonadIO (liftIO))
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
    , ("toJSON", mkFunction hsonToJSON)
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

hsonToJSON :: FunctionDefinition
hsonToJSON [arg, Number n] = do
  spaces <- intFromNumber n
  case T.decodeUtf8' $
    AP.encodePretty'
      (encodePrettyConfig spaces)
      arg of
    Left err -> throwError $ JSONEncodingError err
    Right t -> return $ String $ T.toStrict t
hsonToJSON [_, arg] = throwError $ UnexpectedType "number" (showType arg)
hsonToJSON [arg] = case T.decodeUtf8' $ A.encode arg of
  Left err -> throwError $ JSONEncodingError err
  Right t -> return $ String $ T.toStrict t
hsonToJSON args = throwError $ VariadicArgCount 1 2 args

hsonToString :: FunctionDefinition
hsonToString [arg] = return $ String $ showValue arg
hsonToString args = throwError $ ArgumentCount 1 args

hsonLength :: FunctionDefinition
hsonLength [Array arr] = return $ Number $ fromIntegral $ V.length arr
hsonLength [String str] = return $ Number $ fromIntegral $ T.length str
hsonLength [arg] = throwError $ UnexpectedType "array or string" (showType arg)
hsonLength args = throwError $ ArgumentCount 1 args

-- Returns the value at the index n. If n is negative, the checked index is index + n. If n is out of bounds, returns Null.
hsonAt :: FunctionDefinition
hsonAt [arg, Number n] = do
  idx <- indexFromNumber n arg
  case arg of
    Array arr -> case arr V.!? idx of
      Just v -> return v
      Nothing -> return Null
    String str -> do
      result <- liftIO $ try $ evaluate $ T.index str idx
      case result :: Either SomeException Char of
        Right result -> return $ String $ T.singleton result
        Left _ -> return Null
    _ -> throwError $ UnexpectedType "array or string" (showType arg)
hsonAt [_, arg] = throwError $ UnexpectedType "number" (showType arg)
hsonAt args = throwError $ ArgumentCount 2 args

hsonReverse :: FunctionDefinition
hsonReverse [Array arr] = return $ Array $ V.reverse arr
hsonReverse [String str] = return $ String $ T.reverse str
hsonReverse [arg] = throwError $ UnexpectedType "array or string" (showType arg)
hsonReverse args = throwError $ ArgumentCount 1 args

{-# LANGUAGE OverloadedStrings #-}

module BuiltIn.Function where

import BuiltIn.Helpers
import Control.Exception
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Error.Class
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import Data.List
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Generic as MV
import HSONValue
import JSONParser

builtInFunctions :: Map.Map T.Text HSONValue
builtInFunctions =
  Map.fromList
    [ ("keys", mkFunction keys)
    , ("values", mkFunction values)
    , ("hasProperty", mkFunction hasProperty)
    , ("toJSON", mkFunction hsonToJSON)
    , ("toString", mkFunction hsonToString)
    , ("length", mkFunction hsonLength)
    , ("at", mkFunction hsonAt)
    , ("reverse", mkFunction hsonReverse)
    , ("map", mkFunction hsonMap)
    , ("filter", mkFunction hsonFilter)
    , ("reduce", mkFunction hsonReduce)
    , ("every", mkFunction hsonEvery)
    , ("some", mkFunction hsonSome)
    , ("find", mkFunction hsonFind)
    , ("sort", mkFunction hsonSort)
    , ("insert", mkFunction hsonInsert)
    , ("splice", mkFunction hsonSplice)
    , ("with", mkFunction hsonWith)
    , ("join", mkFunction hsonJoin)
    , ("push", mkFunction hsonPush)
    , ("unshift", mkFunction hsonUnshift)
    , ("pop", mkFunction hsonPop)
    , ("shift", mkFunction hsonShift)
    , ("includes", mkFunction hsonIncludes)
    , ("split", mkFunction hsonSplit)
    , ("words", mkFunction hsonWords)
    , ("lines", mkFunction hsonLines)
    , ("startsWith", mkFunction hsonStartsWith)
    , ("endsWith", mkFunction hsonEndsWith)
    , ("replace", mkFunction hsonReplace)
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

hsonMap :: FunctionDefinition
hsonMap [Array arr, Lambda (Func f) env] = Array <$> local (const env) (V.mapM (f . L.singleton) arr)
hsonMap [Array _, arg] = throwError $ UnexpectedType "lambda" (showType arg)
hsonMap [arg, _] = throwError $ UnexpectedType "array" (showType arg)
hsonMap args = throwError $ ArgumentCount 2 args

hsonFilter :: FunctionDefinition
hsonFilter [Array arr, Lambda f env] = Array <$> local (const env) (V.filterM (returnsTruthy f) arr)
hsonFilter [Array _, arg] = throwError $ UnexpectedType "lambda" (showType arg)
hsonFilter [arg, _] = throwError $ UnexpectedType "array" (showType arg)
hsonFilter args = throwError $ ArgumentCount 2 args

hsonReduce :: FunctionDefinition
hsonReduce [Array arr, Lambda (Func f) env, initial] = local (const env) (V.foldM (\a b -> f [a, b]) initial arr)
hsonReduce [Array _, arg, _] = throwError $ UnexpectedType "lambda" (showType arg)
hsonReduce [arg, _, _] = throwError $ UnexpectedType "array" (showType arg)
hsonReduce args = throwError $ ArgumentCount 2 args

hsonEvery :: FunctionDefinition
hsonEvery [Array arr, Lambda f env] = local (const env) (Bool <$> vAllM (returnsTruthy f) arr)
hsonEvery [Array _, arg] = throwError $ UnexpectedType "lambda" (showType arg)
hsonEvery [arg, _] = throwError $ UnexpectedType "array" (showType arg)
hsonEvery args = throwError $ ArgumentCount 2 args

hsonSome :: FunctionDefinition
hsonSome [Array arr, Lambda f env] = local (const env) (Bool <$> vAnyM (returnsTruthy f) arr)
hsonSome [Array _, arg] = throwError $ UnexpectedType "lambda" (showType arg)
hsonSome [arg, _] = throwError $ UnexpectedType "array" (showType arg)
hsonSome args = throwError $ ArgumentCount 2 args

hsonFind :: FunctionDefinition
hsonFind [Array arr, Lambda f env] = local (const env) (fromMaybe Null <$> vFindM (returnsTruthy f) arr)
hsonFind [Array _, arg] = throwError $ UnexpectedType "lambda" (showType arg)
hsonFind [arg, _] = throwError $ UnexpectedType "lambda" (showType arg)
hsonFind args = throwError $ ArgumentCount 2 args

hsonSort :: FunctionDefinition
hsonSort [Array arr] = return $ Array $ V.map fromSorted $ MV.modify VA.sort (V.map Asc arr)
hsonSort [Array arr, String s] = case s of
  "asc" -> return $ Array $ V.map fromSorted $ MV.modify VA.sort (V.map Asc arr)
  "dsc" -> return $ Array $ V.map fromSorted $ MV.modify VA.sort (V.map Desc arr)
  arg ->
    throwError $ UnexpectedType "\"asc\" or \"dsc\"" (T.concat ["\"", arg, "\""])
hsonSort [arg] = throwError $ UnexpectedType "string" (showType arg)
hsonSort [Array arr, String s, String key] = case s of
  "abk" ->
    return $ Array $ V.map fromSorted $ MV.modify VA.sort (V.map (AscByKey key) arr)
  "dbk" ->
    return $
      Array $
        V.map fromSorted $
          MV.modify VA.sort (V.map (DescByKey key) arr)
  arg ->
    throwError $ UnexpectedType "\"abk\" or \"dbk\"" (T.concat ["\"", arg, "\""])
hsonSort [Array _, String _, arg] = throwError $ UnexpectedType "string" (showType arg)
hsonSort (Array _ : arg : _) = throwError $ UnexpectedType "string" (showType arg)
hsonSort (arg : _) = throwError $ UnexpectedType "array" (showType arg)
hsonSort args = throwError $ VariadicArgCount 1 3 args

hsonInsert :: FunctionDefinition
hsonInsert [Array arr, Number n, v] = do
  index <- indexFromNumber n (Array arr)
  let (x, y) = V.splitAt index arr
   in return $ Array $ x <> V.singleton v <> y
hsonInsert [Array arr, arg, _] = throwError $ UnexpectedType "integer" (showType arg)
hsonInsert [arg, _, _] = throwError $ UnexpectedType "array" (showType arg)
hsonInsert args = throwError $ ArgumentCount 3 args

hsonSplice :: FunctionDefinition
hsonSplice [Array arr, Number n] = do
  index <- indexFromNumber n (Array arr)
  return $ Array $ V.take index arr
hsonSplice [Array arr, Number idx, Number n] = do
  index <- indexFromNumber idx (Array arr)
  dropCount <- intFromNumber n
  let (x, y) = V.splitAt index arr
   in return $ Array $ x <> V.drop dropCount y
hsonSplice [Array arr, Number idx, Number n, Array inserts] = do
  index <- indexFromNumber idx (Array arr)
  dropCount <- intFromNumber n
  let (x, y) = V.splitAt index arr
   in return $ Array $ x <> inserts <> V.drop dropCount y
hsonSplice [Array arr, Number _, Number _, arg] = throwError $ UnexpectedType "array" (showType arg)
hsonSplice (Array arr : Number _ : arg : _) = throwError $ UnexpectedType "integer" (showType arg)
hsonSplice (Array arr : arg : _) = throwError $ UnexpectedType "integer" (showType arg)
hsonSplice (arg : _) = throwError $ UnexpectedType "array" (showType arg)
hsonSplice args = throwError $ VariadicArgCount 2 4 args

hsonWith :: FunctionDefinition
hsonWith [Array arr, Number n, v] = do
  index <- indexFromNumber n (Array arr)
  if index `inBounds` arr
    then return $ Array $ arr V.// [(index, v)]
    else throwError $ IndexOutOfBounds' index
hsonWith [Array arr, arg, _] = throwError $ UnexpectedType "integer" (showType arg)
hsonWith [arg, _, _] = throwError $ UnexpectedType "array" (showType arg)
hsonWith args = throwError $ ArgumentCount 3 args

hsonJoin :: FunctionDefinition
hsonJoin [Array arr, String s] = return $ String $ T.concat $ intersperse s $ V.toList $ V.map showValue arr
hsonJoin [Array _, arg] = throwError $ UnexpectedType "string" (showType arg)
hsonJoin [arg, _] = throwError $ UnexpectedType "array" (showType arg)
hsonJoin args = throwError $ ArgumentCount 2 args

hsonPush :: FunctionDefinition
hsonPush [Array arr, v] = return $ Array $ V.snoc arr v
hsonPush [arg, _] = throwError $ UnexpectedType "array" (showType arg)
hsonPush args = throwError $ ArgumentCount 2 args

hsonUnshift :: FunctionDefinition
hsonUnshift [Array arr, v] = return $ Array $ V.cons v arr
hsonUnshift [arg, _] = throwError $ UnexpectedType "array" (showType arg)
hsonUnshift args = throwError $ ArgumentCount 2 args

hsonPop :: FunctionDefinition
hsonPop [Array arr] = return $ Array $ V.init arr
hsonPop [arg] = throwError $ UnexpectedType "array" (showType arg)
hsonPop args = throwError $ ArgumentCount 1 args

hsonShift :: FunctionDefinition
hsonShift [Array arr] = return $ Array $ V.tail arr
hsonShift [arg] = throwError $ UnexpectedType "array" (showType arg)
hsonShift args = throwError $ ArgumentCount 1 args

hsonIncludes :: FunctionDefinition
hsonIncludes [String haystack, String needle] = return $ Bool $ needle `T.isInfixOf` haystack
hsonIncludes [String _, arg] = throwError $ UnexpectedType "string" (showType arg)
hsonIncludes [arg, _] = throwError $ UnexpectedType "string" (showType arg)
hsonIncludes args = throwError $ ArgumentCount 2 args

hsonSplit :: FunctionDefinition
hsonSplit [String str, String splitter] = return $ Array $ V.map String $ V.fromList $ T.splitOn splitter str
hsonSplit [String _, arg] = throwError $ UnexpectedType "string" (showType arg)
hsonSplit [arg, _] = throwError $ UnexpectedType "string" (showType arg)
hsonSplit args = throwError $ ArgumentCount 2 args

hsonWords :: FunctionDefinition
hsonWords [String str] = return $ Array $ V.map String $ V.fromList $ T.words str
hsonWords args = throwError $ ArgumentCount 1 args

hsonLines :: FunctionDefinition
hsonLines [String str] = return $ Array $ V.map String $ V.fromList $ T.lines str
hsonLines args = throwError $ ArgumentCount 1 args

hsonStartsWith :: FunctionDefinition
hsonStartsWith [String str, String prefix] = return $ Bool $ prefix `T.isPrefixOf` str
hsonStartsWith [String _, arg] = throwError $ UnexpectedType "string" (showType arg)
hsonStartsWith [arg, _] = throwError $ UnexpectedType "string" (showType arg)
hsonStartsWith args = throwError $ ArgumentCount 2 args

hsonEndsWith :: FunctionDefinition
hsonEndsWith [String str, String suffix] = return $ Bool $ suffix `T.isSuffixOf` str
hsonEndsWith [String _, arg] = throwError $ UnexpectedType "string" (showType arg)
hsonEndsWith [arg, _] = throwError $ UnexpectedType "string" (showType arg)
hsonEndsWith args = throwError $ ArgumentCount 2 args

hsonReplace :: FunctionDefinition
hsonReplace [String haystack, String needle, String replacement] = return $ String $ T.replace needle replacement haystack
hsonReplace [String _, String _, arg] = throwError $ UnexpectedType "string" (showType arg)
hsonReplace [String _, arg, _] = throwError $ UnexpectedType "string" (showType arg)
hsonReplace [arg, _, _] = throwError $ UnexpectedType "string" (showType arg)
hsonReplace args = throwError $ ArgumentCount 3 args

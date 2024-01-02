{-# LANGUAGE OverloadedStrings #-}

module BuiltIn.Array where

import BuiltIn.General
import BuiltIn.Helpers
import Control.Monad.Except
import Control.Monad.Reader.Class
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Generic as MV
import HSONValue

arrayMethods :: Map.Map T.Text HSONValue
arrayMethods =
  Map.fromList
    [ ("length", mkMethod hsonLength)
    , ("at", mkMethod hsonAt)
    , ("map", mkMethod hsonMap)
    , ("filter", mkMethod hsonFilter)
    , ("reduce", mkMethod hsonReduce)
    , ("every", mkMethod hsonEvery)
    , ("some", mkMethod hsonSome)
    , ("find", mkMethod hsonFind)
    , ("sort", mkMethod hsonSort)
    , ("insert", mkMethod hsonInsert)
    , ("splice", mkMethod hsonSplice)
    , ("push", mkMethod hsonPush)
    , ("unshift", mkMethod hsonUnshift)
    , ("pop", mkMethod hsonPop)
    , ("shift", mkMethod hsonShift)
    , ("with", mkMethod hsonWith)
    , ("join", mkMethod hsonJoin)
    , ("reverse", mkMethod hsonReverse)
    , ("toString", mkMethod hsonToString)
    , ("toJSON", mkMethod hsonToJSON)
    ]

hsonMap :: MethodDefinition
hsonMap (Array arr) [Lambda (Func f) env] = Array <$> local (const env) (V.mapM (f . singleton) arr)
hsonMap _ [arg] = throwError $ UnexpectedType "lambda" (showType arg)
hsonMap _ args = throwError $ ArgumentCount 1 args

hsonFilter :: MethodDefinition
hsonFilter (Array arr) [Lambda f env] = Array <$> local (const env) (V.filterM (returnsTruthy f) arr)
hsonFilter _ [arg] = throwError $ UnexpectedType "lambda" (showType arg)
hsonFilter _ args = throwError $ ArgumentCount 1 args

hsonReduce :: MethodDefinition
hsonReduce (Array arr) [Lambda (Func f) env, initial] = local (const env) (V.foldM (\a b -> f [a, b]) initial arr)
hsonReduce (Array arr) [arg, _] = throwError $ UnexpectedType "lambda" (showType arg)
hsonReduce _ args = throwError $ ArgumentCount 2 args

hsonEvery :: MethodDefinition
hsonEvery (Array arr) [Lambda f env] = local (const env) (Bool <$> vAllM (returnsTruthy f) arr)
hsonEvery (Array arr) [arg] = throwError $ UnexpectedType "lambda" (showType arg)
hsonEvery _ args = throwError $ ArgumentCount 1 args

hsonSome :: MethodDefinition
hsonSome (Array arr) [Lambda f env] = local (const env) (Bool <$> vAnyM (returnsTruthy f) arr)
hsonSome (Array arr) [arg] = throwError $ UnexpectedType "lambda" (showType arg)
hsonSome _ args = throwError $ ArgumentCount 1 args

hsonFind :: MethodDefinition
hsonFind (Array arr) [Lambda f env] = local (const env) (fromMaybe Null <$> vFindM (returnsTruthy f) arr)
hsonFind (Array arr) [arg] = throwError $ UnexpectedType "lambda" (showType arg)
hsonFind _ args = throwError $ ArgumentCount 1 args

hsonSort :: MethodDefinition
hsonSort (Array arr) [] = return $ Array $ V.map fromSorted $ MV.modify VA.sort (V.map Asc arr)
hsonSort (Array arr) [String s] = case s of
  "asc" -> return $ Array $ V.map fromSorted $ MV.modify VA.sort (V.map Asc arr)
  "dsc" -> return $ Array $ V.map fromSorted $ MV.modify VA.sort (V.map Desc arr)
  arg ->
    throwError $ UnexpectedType "\"asc\" or \"dsc\"" (T.concat ["\"", arg, "\""])
hsonSort _ [arg] = throwError $ UnexpectedType "string" (showType arg)
hsonSort (Array arr) [String s, String key] = case s of
  "abk" ->
    return $ Array $ V.map fromSorted $ MV.modify VA.sort (V.map (AscByKey key) arr)
  "dbk" ->
    return $
      Array $
        V.map fromSorted $
          MV.modify VA.sort (V.map (DescByKey key) arr)
  arg ->
    throwError $ UnexpectedType "\"abk\" or \"dbk\"" (T.concat ["\"", arg, "\""])
hsonSort _ [arg, String _] = throwError $ UnexpectedType "string" (showType arg)
hsonSort _ [_, arg] = throwError $ UnexpectedType "string" (showType arg)
hsonSort _ args = throwError $ VariadicArgCount 0 2 args

hsonInsert :: MethodDefinition
hsonInsert (Array arr) [Number n, v] = do
  index <- indexFromNumber n (Array arr)
  let (x, y) = V.splitAt index arr
   in return $ Array $ x <> V.singleton v <> y
hsonInsert (Array arr) [arg, _] = throwError $ UnexpectedType "integer" (showType arg)
hsonInsert _ args = throwError $ ArgumentCount 2 args

hsonSplice :: MethodDefinition
hsonSplice (Array arr) [Number n] = do
  index <- indexFromNumber n (Array arr)
  return $ Array $ V.take index arr
hsonSplice (Array arr) [Number idx, Number n] = do
  index <- indexFromNumber idx (Array arr)
  dropCount <- intFromNumber n
  let (x, y) = V.splitAt index arr
   in return $ Array $ x <> V.drop dropCount y
hsonSplice (Array arr) [Number idx, Number n, Array inserts] = do
  index <- indexFromNumber idx (Array arr)
  dropCount <- intFromNumber n
  let (x, y) = V.splitAt index arr
   in return $ Array $ x <> inserts <> V.drop dropCount y
hsonSplice (Array arr) (Number _ : Number _ : arg : _) = throwError $ UnexpectedType "array" (showType arg)
hsonSplice (Array arr) (Number _ : arg : _) = throwError $ UnexpectedType "integer" (showType arg)
hsonSplice (Array arr) (arg : _) = throwError $ UnexpectedType "integer" (showType arg)
hsonSplice _ args = throwError $ VariadicArgCount 1 3 args

hsonWith :: MethodDefinition
hsonWith (Array arr) [Number n, v] = do
  index <- indexFromNumber n (Array arr)
  if index `inBounds` arr
    then return $ Array $ arr V.// [(index, v)]
    else throwError $ IndexOutOfBounds' index
hsonWith (Array arr) [arg, _] = throwError $ UnexpectedType "integer" (showType arg)
hsonWith _ args = throwError $ ArgumentCount 2 args

hsonJoin :: MethodDefinition
hsonJoin (Array arr) [String s] = return $ String $ T.concat $ intersperse s $ V.toList $ V.map showValue arr
hsonJoin _ args = throwError $ ArgumentCount 1 args

hsonPush :: MethodDefinition
hsonPush (Array arr) [v] = return $ Array $ V.snoc arr v
hsonPush _ args = throwError $ ArgumentCount 1 args

hsonUnshift :: MethodDefinition
hsonUnshift (Array arr) [v] = return $ Array $ V.cons v arr
hsonUnshift _ args = throwError $ ArgumentCount 1 args

hsonPop :: MethodDefinition
hsonPop (Array arr) [] = return $ Array $ V.init arr
hsonPop _ args = throwError $ ArgumentCount 0 args

hsonShift :: MethodDefinition
hsonShift (Array arr) [] = return $ Array $ V.tail arr
hsonShift _ args = throwError $ ArgumentCount 0 args

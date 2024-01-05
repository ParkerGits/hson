{-# LANGUAGE OverloadedStrings #-}

module BuiltIn.Array where

import BuiltIn.Function
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
    [ ("length", mkMethod arrayLength)
    , ("at", mkMethod arrayAt)
    , ("map", mkMethod arrayMap)
    , ("filter", mkMethod arrayFilter)
    , ("reduce", mkMethod arrayReduce)
    , ("every", mkMethod arrayEvery)
    , ("some", mkMethod arraySome)
    , ("find", mkMethod arrayFind)
    , ("sort", mkMethod arraySort)
    , ("insert", mkMethod arrayInsert)
    , ("splice", mkMethod arraySplice)
    , ("push", mkMethod arrayPush)
    , ("unshift", mkMethod arrayUnshift)
    , ("pop", mkMethod arrayPop)
    , ("shift", mkMethod arrayShift)
    , ("with", mkMethod arrayWith)
    , ("join", mkMethod arrayJoin)
    , ("reverse", mkMethod arrayReverse)
    , ("toString", mkMethod arrayToString)
    , ("toJSON", mkMethod arrayToJSON)
    ]

arrayLength :: MethodDefinition
arrayLength this [] = hsonLength [this]
arrayLength _ args = throwError $ ArgumentCount 0 args

arrayToString :: MethodDefinition
arrayToString this [] = hsonToString [this]
arrayToString _ args = throwError $ ArgumentCount 0 args

arrayToJSON :: MethodDefinition
arrayToJSON this [Number n] = hsonToJSON [this, Number n]
arrayToJSON this [] = hsonToJSON [this]
arrayToJSON _ [arg] = throwError $ UnexpectedType "number" (showType arg)
arrayToJSON _ args = throwError $ VariadicArgCount 0 1 args

arrayAt :: MethodDefinition
arrayAt this [Number n] = hsonToJSON [this, Number n]
arrayAt _ [arg] = throwError $ UnexpectedType "number" (showType arg)
arrayAt _ args = throwError $ ArgumentCount 1 args

arrayReverse :: MethodDefinition
arrayReverse this [] = hsonToJSON [this]
arrayReverse _ args = throwError $ ArgumentCount 0 args

arrayMap :: MethodDefinition
arrayMap (Array arr) [Lambda (Func f) env] = Array <$> local (const env) (V.mapM (f . singleton) arr)
arrayMap _ [arg] = throwError $ UnexpectedType "lambda" (showType arg)
arrayMap _ args = throwError $ ArgumentCount 1 args

arrayFilter :: MethodDefinition
arrayFilter (Array arr) [Lambda f env] = Array <$> local (const env) (V.filterM (returnsTruthy f) arr)
arrayFilter _ [arg] = throwError $ UnexpectedType "lambda" (showType arg)
arrayFilter _ args = throwError $ ArgumentCount 1 args

arrayReduce :: MethodDefinition
arrayReduce (Array arr) [Lambda (Func f) env, initial] = local (const env) (V.foldM (\a b -> f [a, b]) initial arr)
arrayReduce (Array arr) [arg, _] = throwError $ UnexpectedType "lambda" (showType arg)
arrayReduce _ args = throwError $ ArgumentCount 2 args

arrayEvery :: MethodDefinition
arrayEvery (Array arr) [Lambda f env] = local (const env) (Bool <$> vAllM (returnsTruthy f) arr)
arrayEvery (Array arr) [arg] = throwError $ UnexpectedType "lambda" (showType arg)
arrayEvery _ args = throwError $ ArgumentCount 1 args

arraySome :: MethodDefinition
arraySome (Array arr) [Lambda f env] = local (const env) (Bool <$> vAnyM (returnsTruthy f) arr)
arraySome (Array arr) [arg] = throwError $ UnexpectedType "lambda" (showType arg)
arraySome _ args = throwError $ ArgumentCount 1 args

arrayFind :: MethodDefinition
arrayFind (Array arr) [Lambda f env] = local (const env) (fromMaybe Null <$> vFindM (returnsTruthy f) arr)
arrayFind (Array arr) [arg] = throwError $ UnexpectedType "lambda" (showType arg)
arrayFind _ args = throwError $ ArgumentCount 1 args

arraySort :: MethodDefinition
arraySort (Array arr) [] = return $ Array $ V.map fromSorted $ MV.modify VA.sort (V.map Asc arr)
arraySort (Array arr) [String s] = case s of
  "asc" -> return $ Array $ V.map fromSorted $ MV.modify VA.sort (V.map Asc arr)
  "dsc" -> return $ Array $ V.map fromSorted $ MV.modify VA.sort (V.map Desc arr)
  arg ->
    throwError $ UnexpectedType "\"asc\" or \"dsc\"" (T.concat ["\"", arg, "\""])
arraySort _ [arg] = throwError $ UnexpectedType "string" (showType arg)
arraySort (Array arr) [String s, String key] = case s of
  "abk" ->
    return $ Array $ V.map fromSorted $ MV.modify VA.sort (V.map (AscByKey key) arr)
  "dbk" ->
    return $
      Array $
        V.map fromSorted $
          MV.modify VA.sort (V.map (DescByKey key) arr)
  arg ->
    throwError $ UnexpectedType "\"abk\" or \"dbk\"" (T.concat ["\"", arg, "\""])
arraySort _ [arg, String _] = throwError $ UnexpectedType "string" (showType arg)
arraySort _ [_, arg] = throwError $ UnexpectedType "string" (showType arg)
arraySort _ args = throwError $ VariadicArgCount 0 2 args

arrayInsert :: MethodDefinition
arrayInsert (Array arr) [Number n, v] = do
  index <- indexFromNumber n (Array arr)
  let (x, y) = V.splitAt index arr
   in return $ Array $ x <> V.singleton v <> y
arrayInsert (Array arr) [arg, _] = throwError $ UnexpectedType "integer" (showType arg)
arrayInsert _ args = throwError $ ArgumentCount 2 args

arraySplice :: MethodDefinition
arraySplice (Array arr) [Number n] = do
  index <- indexFromNumber n (Array arr)
  return $ Array $ V.take index arr
arraySplice (Array arr) [Number idx, Number n] = do
  index <- indexFromNumber idx (Array arr)
  dropCount <- intFromNumber n
  let (x, y) = V.splitAt index arr
   in return $ Array $ x <> V.drop dropCount y
arraySplice (Array arr) [Number idx, Number n, Array inserts] = do
  index <- indexFromNumber idx (Array arr)
  dropCount <- intFromNumber n
  let (x, y) = V.splitAt index arr
   in return $ Array $ x <> inserts <> V.drop dropCount y
arraySplice (Array arr) (Number _ : Number _ : arg : _) = throwError $ UnexpectedType "array" (showType arg)
arraySplice (Array arr) (Number _ : arg : _) = throwError $ UnexpectedType "integer" (showType arg)
arraySplice (Array arr) (arg : _) = throwError $ UnexpectedType "integer" (showType arg)
arraySplice _ args = throwError $ VariadicArgCount 1 3 args

arrayWith :: MethodDefinition
arrayWith (Array arr) [Number n, v] = do
  index <- indexFromNumber n (Array arr)
  if index `inBounds` arr
    then return $ Array $ arr V.// [(index, v)]
    else throwError $ IndexOutOfBounds' index
arrayWith (Array arr) [arg, _] = throwError $ UnexpectedType "integer" (showType arg)
arrayWith _ args = throwError $ ArgumentCount 2 args

arrayJoin :: MethodDefinition
arrayJoin (Array arr) [String s] = return $ String $ T.concat $ intersperse s $ V.toList $ V.map showValue arr
arrayJoin _ args = throwError $ ArgumentCount 1 args

arrayPush :: MethodDefinition
arrayPush (Array arr) [v] = return $ Array $ V.snoc arr v
arrayPush _ args = throwError $ ArgumentCount 1 args

arrayUnshift :: MethodDefinition
arrayUnshift (Array arr) [v] = return $ Array $ V.cons v arr
arrayUnshift _ args = throwError $ ArgumentCount 1 args

arrayPop :: MethodDefinition
arrayPop (Array arr) [] = return $ Array $ V.init arr
arrayPop _ args = throwError $ ArgumentCount 0 args

arrayShift :: MethodDefinition
arrayShift (Array arr) [] = return $ Array $ V.tail arr
arrayShift _ args = throwError $ ArgumentCount 0 args

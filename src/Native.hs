{-# LANGUAGE OverloadedStrings #-}

module Native where

import Control.Monad.Except
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class
import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty (defConfig)
import qualified Data.Aeson.Encode.Pretty as AP
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Generic as MV
import HSONValue
import JSONParser

mkMethod :: (HSONValue -> [HSONValue] -> Eval HSONValue) -> HSONValue
mkMethod f = Method (Func . f)

arrayMethods :: Map.Map T.Text HSONValue
arrayMethods =
  Map.fromList
    [ ("length", mkMethod hsonLength)
    , ("at", mkMethod hsonAt)
    , ("map", mkMethod hsonMap)
    , ("filter", mkMethod hsonFilter)
    , ("reduce", mkMethod hsonReduce)
    , ("some", mkMethod hsonSome)
    , ("all", mkMethod hsonAll)
    , ("find", mkMethod hsonFind)
    , ("sort", mkMethod hsonSort)
    , ("toString", mkMethod hsonToString)
    , ("toJSON", mkMethod hsonToJSON)
    ]

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
hsonToJSON this [Number n] = case floatingOrInteger n of
  Left float -> throwError $ UnexpectedType "integer" "floating"
  Right spaces -> case T.decodeUtf8' $
    AP.encodePretty'
      ( AP.Config
          { AP.confTrailingNewline = False
          , AP.confNumFormat = AP.Generic
          , AP.confIndent = AP.Spaces spaces
          , AP.confCompare = mempty
          }
      )
      this of
    Left err -> throwError $ JSONEncodingError err
    Right t -> return $ String $ T.toStrict t
hsonToJSON _ [arg] = throwError $ UnexpectedType "number" (showType arg)
hsonToJSON _ args = throwError $ VariadicArgCount 0 1 args

hsonAt :: HSONValue -> [HSONValue] -> Eval HSONValue
hsonAt this [Number n] =
  case floatingOrInteger n of
    Left float -> throwError $ UnexpectedType "integer" "floating"
    Right idx -> case this of
      Array arr -> case arr V.!? idx of
        Just v -> return v
        Nothing -> return Null
hsonAt _ [arg] = throwError $ UnexpectedType "number" (showType arg)
hsonAt _ args = throwError $ ArgumentCount 1 args

hsonMap :: HSONValue -> [HSONValue] -> Eval HSONValue
hsonMap this [Lambda (Func f) env] =
  case this of
    Array arr -> Array <$> local (const env) (V.mapM (f . singleton) arr)
hsonMap _ [arg] = throwError $ UnexpectedType "lambda" (showType arg)
hsonMap _ args = throwError $ ArgumentCount 1 args

hsonFilter :: HSONValue -> [HSONValue] -> Eval HSONValue
hsonFilter this [Lambda f env] =
  case this of
    Array arr -> Array <$> local (const env) (V.filterM (returnsTruthy f) arr)
hsonFilter _ [arg] = throwError $ UnexpectedType "lambda" (showType arg)
hsonFilter _ args = throwError $ ArgumentCount 1 args

hsonReduce :: HSONValue -> [HSONValue] -> Eval HSONValue
hsonReduce this [Lambda (Func f) env, initial] =
  case this of
    Array arr -> local (const env) (V.foldM (\a b -> f [a, b]) initial arr)
hsonReduce this [arg, _] = throwError $ UnexpectedType "lambda" (showType arg)
hsonReduce _ args = throwError $ ArgumentCount 2 args

hsonSome :: HSONValue -> [HSONValue] -> Eval HSONValue
hsonSome this [Lambda f env] =
  case this of
    Array arr -> local (const env) (Bool <$> vAnyM (returnsTruthy f) arr)
hsonSome this [arg] = throwError $ UnexpectedType "lambda" (showType arg)
hsonSome _ args = throwError $ ArgumentCount 1 args

hsonAll :: HSONValue -> [HSONValue] -> Eval HSONValue
hsonAll this [Lambda f env] =
  case this of
    Array arr -> local (const env) (Bool <$> vAllM (returnsTruthy f) arr)
hsonAll this [arg] = throwError $ UnexpectedType "lambda" (showType arg)
hsonAll _ args = throwError $ ArgumentCount 1 args

hsonFind :: HSONValue -> [HSONValue] -> Eval HSONValue
hsonFind this [Lambda f env] =
  case this of
    Array arr -> local (const env) (fromMaybe Null <$> vFindM (returnsTruthy f) arr)
hsonFind this [arg] = throwError $ UnexpectedType "lambda" (showType arg)
hsonFind _ args = throwError $ ArgumentCount 1 args

hsonSort :: HSONValue -> [HSONValue] -> Eval HSONValue
hsonSort this [] = case this of
  Array arr -> return $ Array $ V.map fromSorted $ MV.modify VA.sort (V.map Asc arr)
hsonSort this [String s] = case this of
  Array arr -> case s of
    "asc" -> return $ Array $ V.map fromSorted $ MV.modify VA.sort (V.map Asc arr)
    "dsc" -> return $ Array $ V.map fromSorted $ MV.modify VA.sort (V.map Desc arr)
    arg ->
      throwError $ UnexpectedType "\"asc\" or \"dsc\"" (T.concat ["\"", arg, "\""])
hsonSort _ [arg] = throwError $ UnexpectedType "string" (showType arg)
hsonSort this [String s, String key] = case this of
  Array arr -> case s of
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

showType :: HSONValue -> T.Text
showType (Lambda _ _) = "lambda"
showType (Array _) = "array"
showType (Object _) = "object"
showType (String _) = "string"
showType (Number _) = "number"
showType (Bool _) = "bool"
showType Null = "null"

isTruthy :: HSONValue -> Bool
isTruthy (Number v) = v /= 0
isTruthy (String v) = not $ T.null v
isTruthy (Bool v) = v
isTruthy Null = False
isTruthy _ = True

returnsTruthy :: Func -> HSONValue -> Eval Bool
returnsTruthy (Func f) x = isTruthy <$> f (singleton x)

vAnyM :: (Monad m) => (a -> m Bool) -> V.Vector a -> m Bool
vAnyM p = V.foldr ((||^) . p) (pure False)

vAllM :: (Monad m) => (a -> m Bool) -> V.Vector a -> m Bool
vAllM p = V.foldr ((&&^) . p) (pure True)

vFindM :: (Monad m) => (a -> m Bool) -> V.Vector a -> m (Maybe a)
vFindM p = V.foldr (\x -> ifM (p x) (return $ Just x)) (return Nothing)

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f

notM :: (Functor m) => m Bool -> m Bool
notM = fmap not

(||^) :: (Monad m) => m Bool -> m Bool -> m Bool
(||^) a = ifM a (pure True)

(&&^) :: (Monad m) => m Bool -> m Bool -> m Bool
(&&^) a b = ifM a b (pure False)

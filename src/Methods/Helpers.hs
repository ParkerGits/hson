{-# LANGUAGE OverloadedStrings #-}

module Methods.Helpers where

import Control.Monad.Except
import Data.List
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Vector as V
import HSONValue

mkMethod :: (HSONValue -> [HSONValue] -> Eval HSONValue) -> HSONValue
mkMethod f = Method (Func . f)

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

inBounds :: Int -> V.Vector HSONValue -> Bool
inBounds i arr = i >= 0 && i < V.length arr

indexFromNumber :: Scientific -> HSONValue -> Eval Int
indexFromNumber n (Array arr) = do
  index <- intFromNumber n
  return $ if index >= 0 then index else index + V.length arr

intFromNumber :: Scientific -> Eval Int
intFromNumber n =
  case floatingOrInteger n of
    Left float -> throwError $ UnexpectedType "integer" "floating"
    Right int -> return int

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

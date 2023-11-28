{-# LANGUAGE OverloadedStrings #-}

module NativeFunction
  ( someFunc,
  )
where

import           Data.Aeson                 (Value (Array, Number, Object, String),
                                             decode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.Aeson.Key             (fromText)
import           Data.Aeson.KeyMap          (member)
import           Data.ByteString            (pack, unpack)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char                  (toUpper)
import           Data.Maybe                 (fromJust)
import           Data.Scientific            (Scientific)
import qualified Data.Text                  as T (Text, map)
import qualified Data.Vector                as V

someFunc :: IO ()
someFunc = do
  contents <- BL.getContents
  let v = (decode contents :: Maybe Value)
  C.putStrLn $ encodePretty $ vFilter (vMember "123") (fromJust v)

vMap :: (Value -> Value) -> Value -> Value
vMap f (Array a)  = Array $ fmap f a
vMap f (Object o) = Object $ fmap f o
vMap _ x          = x

vToUpper :: Value -> Value
vToUpper (String s) = String (T.map toUpper s)
vToUpper x          = x

vAdd :: Scientific -> Value -> Value
vAdd a (Number n) = Number (a + n)
vAdd _ x          = x

vFilter :: (Value -> Bool) -> Value -> Value
vFilter p (Array a) = Array (V.filter p a)
vFilter _ x         = x

vMember :: T.Text -> Value -> Bool
vMember t (Object o) = member (fromText t) o
vMember _ _          = True

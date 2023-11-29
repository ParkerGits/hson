module Main where

import qualified NativeFunction (someFunc)
import           Parser

main :: IO ()
main = do
  NativeFunction.someFunc

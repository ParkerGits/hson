module Main where

import qualified NativeFunction (someFunc)
import qualified Parser         (test)

main :: IO ()
main = do
  NativeFunction.someFunc

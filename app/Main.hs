module Main where

import           Data.Either    (fromLeft)
import           Interpreter
import qualified NativeFunction (someFunc)
import           Parser

main = do
  str <- getContents
  snd $ test str
  evald <- testEval
  case evald of
    Left err  -> print err
    Right val -> print val

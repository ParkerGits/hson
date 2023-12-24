module Main where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Either            (fromLeft)
import           Interpreter
import           Parser

main = do
  str <- getContents
  let v = test str in
    case v of
      Left err  -> print err
      Right val -> do
        -- print val
        result <- testInterpret val
        case result of
          Left v  -> print v
          Right v -> print v

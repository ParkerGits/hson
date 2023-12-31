{-# LANGUAGE OverloadedStrings #-}

module Methods.String where

import qualified Data.Map as Map
import qualified Data.Text as T
import HSONValue
import Methods.General
import Methods.Helpers

stringMethods :: Map.Map T.Text HSONValue
stringMethods =
  Map.fromList
    [ ("length", mkMethod hsonLength)
    , ("at", mkMethod hsonAt)
    , ("reverse", mkMethod hsonReverse)
    , ("toString", mkMethod hsonToString)
    , ("toJSON", mkMethod hsonToJSON)
    ]

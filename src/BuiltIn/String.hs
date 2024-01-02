{-# LANGUAGE OverloadedStrings #-}

module BuiltIn.String where

import BuiltIn.General
import BuiltIn.Helpers
import Control.Monad.Error.Class
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import HSONValue

stringMethods :: Map.Map T.Text HSONValue
stringMethods =
  Map.fromList
    [ ("length", mkMethod hsonLength)
    , ("at", mkMethod hsonAt)
    , ("reverse", mkMethod hsonReverse)
    , ("toString", mkMethod hsonToString)
    , ("toJSON", mkMethod hsonToJSON)
    , ("includes", mkMethod hsonIncludes)
    , ("split", mkMethod hsonSplit)
    , ("words", mkMethod hsonWords)
    , ("lines", mkMethod hsonLines)
    ]

hsonIncludes :: MethodDefinition
hsonIncludes (String haystack) [String needle] = return $ Bool $ needle `T.isInfixOf` haystack
hsonIncludes (String _) [arg] = throwError $ UnexpectedType "string" (showType arg)
hsonIncludes _ args = throwError $ ArgumentCount 1 args

hsonSplit :: MethodDefinition
hsonSplit (String str) [String splitter] = return $ Array $ V.map String $ V.fromList $ T.splitOn splitter str
hsonSplit (String _) [arg] = throwError $ UnexpectedType "string" (showType arg)
hsonSplit _ args = throwError $ ArgumentCount 1 args

hsonWords :: MethodDefinition
hsonWords (String str) [] = return $ Array $ V.map String $ V.fromList $ T.words str
hsonWords _ args = throwError $ ArgumentCount 0 args

hsonLines :: MethodDefinition
hsonLines (String str) [] = return $ Array $ V.map String $ V.fromList $ T.lines str
hsonLines _ args = throwError $ ArgumentCount 0 args

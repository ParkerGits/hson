{-# LANGUAGE OverloadedStrings #-}

module BuiltIn.String where

import BuiltIn.Function
import BuiltIn.Helpers
import Control.Monad.Error.Class
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import HSONValue

stringLength :: MethodDefinition
stringLength this [] = hsonLength [this]
stringLength _ args = throwError $ ArgumentCount 0 args

stringToString :: MethodDefinition
stringToString this [] = hsonToString [this]
stringToString _ args = throwError $ ArgumentCount 0 args

stringToJSON :: MethodDefinition
stringToJSON this [Number n] = hsonToJSON [this, Number n]
stringToJSON this [] = hsonToJSON [this]
stringToJSON _ [arg] = throwError $ UnexpectedType "number" (showType arg)
stringToJSON _ args = throwError $ VariadicArgCount 0 1 args

stringAt :: MethodDefinition
stringAt this [Number n] = hsonAt [this, Number n]
stringAt _ [arg] = throwError $ UnexpectedType "number" (showType arg)
stringAt _ args = throwError $ ArgumentCount 1 args

stringReverse :: MethodDefinition
stringReverse this [] = hsonReverse [this]
stringReverse _ args = throwError $ ArgumentCount 0 args

stringIncludes :: MethodDefinition
stringIncludes (String haystack) [String needle] = hsonIncludes [String haystack, String needle]
stringIncludes (String _) [arg] = throwError $ UnexpectedType "string" (showType arg)
stringIncludes _ args = throwError $ ArgumentCount 1 args

stringSplit :: MethodDefinition
stringSplit (String str) [String splitter] = hsonSplit [String str, String splitter]
stringSplit (String _) [arg] = throwError $ UnexpectedType "string" (showType arg)
stringSplit _ args = throwError $ ArgumentCount 1 args

stringWords :: MethodDefinition
stringWords (String str) [] = hsonWords [String str]
stringWords _ args = throwError $ ArgumentCount 0 args

stringLines :: MethodDefinition
stringLines (String str) [] = hsonLines [String str]
stringLines _ args = throwError $ ArgumentCount 0 args

stringStartsWith :: MethodDefinition
stringStartsWith (String str) [String prefix] = hsonStartsWith [String str, String prefix]
stringStartsWith (String _) [arg] = throwError $ UnexpectedType "string" (showType arg)
stringStartsWith _ args = throwError $ ArgumentCount 1 args

stringEndsWith :: MethodDefinition
stringEndsWith (String str) [String suffix] = hsonEndsWith [String str, String suffix]
stringEndsWith (String _) [arg] = throwError $ UnexpectedType "string" (showType arg)
stringEndsWith _ args = throwError $ ArgumentCount 1 args

stringReplace :: MethodDefinition
stringReplace (String haystack) [String needle, String replacement] = hsonReplace [String haystack, String needle, String replacement]
stringReplace (String _) [String _, arg] = throwError $ UnexpectedType "string" (showType arg)
stringReplace (String _) [arg, _] = throwError $ UnexpectedType "string" (showType arg)
stringReplace _ args = throwError $ ArgumentCount 2 args

stringMethods :: Map.Map T.Text HSONValue
stringMethods =
  Map.fromList
    [ ("length", mkMethod stringLength)
    , ("at", mkMethod stringAt)
    , ("reverse", mkMethod stringReverse)
    , ("toString", mkMethod stringToString)
    , ("toJSON", mkMethod stringToJSON)
    , ("includes", mkMethod stringIncludes)
    , ("split", mkMethod stringSplit)
    , ("words", mkMethod stringWords)
    , ("lines", mkMethod stringLines)
    , ("startsWith", mkMethod stringStartsWith)
    , ("endsWith", mkMethod stringEndsWith)
    , ("replace", mkMethod stringReplace)
    ]

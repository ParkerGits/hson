{-# LANGUAGE OverloadedStrings #-}

module BuiltIn.String where

import BuiltIn.Helpers
import Control.Monad.Error.Class
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import HSONValue

stringMethods :: Map.Map T.Text HSONValue
stringMethods =
  Map.fromList
    -- [ ("length", mkMethod hsonLength)
    -- , ("at", mkMethod hsonAt)
    -- , ("reverse", mkMethod hsonReverse)
    -- , ("toString", mkMethod hsonToString)
    -- , ("toJSON", mkMethod hsonToJSON)
    -- , ("includes", mkMethod hsonIncludes)

    [ ("split", mkMethod hsonSplit)
    , ("words", mkMethod hsonWords)
    , ("lines", mkMethod hsonLines)
    , ("startsWith", mkMethod hsonStartsWith)
    , ("endsWith", mkMethod hsonEndsWith)
    , ("replace", mkMethod hsonReplace)
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

hsonStartsWith :: MethodDefinition
hsonStartsWith (String str) [String prefix] = return $ Bool $ prefix `T.isPrefixOf` str
hsonStartsWith (String _) [arg] = throwError $ UnexpectedType "string" (showType arg)
hsonStartsWith _ args = throwError $ ArgumentCount 1 args

hsonEndsWith :: MethodDefinition
hsonEndsWith (String str) [String suffix] = return $ Bool $ suffix `T.isSuffixOf` str
hsonEndsWith (String _) [arg] = throwError $ UnexpectedType "string" (showType arg)
hsonEndsWith _ args = throwError $ ArgumentCount 1 args

hsonReplace :: MethodDefinition
hsonReplace (String haystack) [String needle, String replacement] = return $ String $ T.replace needle replacement haystack
hsonReplace (String _) [String _, arg] = throwError $ UnexpectedType "string" (showType arg)
hsonReplace (String _) [arg, _] = throwError $ UnexpectedType "string" (showType arg)
hsonReplace _ args = throwError $ ArgumentCount 2 args

{-# LANGUAGE OverloadedStrings #-}

module BuiltIn.Function where

import BuiltIn.Helpers
import Control.Monad.Error.Class
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import HSONValue

builtInFunctions :: Map.Map T.Text HSONValue
builtInFunctions = Map.fromList [("keys", mkFunction keys), ("values", mkFunction values)]

keys :: FunctionDefinition
keys [Object o] = return $ Array $ V.map String $ V.fromList $ Map.keys o
keys [arg] = throwError $ UnexpectedType "object" (showType arg)
keys args = throwError $ ArgumentCount 0 args

values :: FunctionDefinition
values [Object o] = return $ Array $ V.fromList $ Map.elems o
values [arg] = throwError $ UnexpectedType "object" (showType arg)
values args = throwError $ ArgumentCount 0 args

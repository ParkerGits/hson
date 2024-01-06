{-# LANGUAGE TemplateHaskell #-}

module BuiltIn.TH where

import BuiltIn.Function
import BuiltIn.Helpers
import Control.Monad (replicateM)
import HSONValue
import Language.Haskell.TH

-- test2 :: String -> [Name] -> Name -> Q [Dec]
-- test2 methodName argNames functionName = do
--   this <- newName "this"
--   let arg = head argNames
--    in do
--         sig <- sigD methodName' (conT ''MethodDefinition)
--         fun <-
--           funD
--             methodName'
--             [ clause
--                 [varP this, listP (toConP argNames)]
--                 ( normalB
--                     ( appE
--                         (varE functionName)
--                         (listE [varE this, appE (conE arg) (varE n)])
--                     )
--                 )
--                 []
--             ]
--         return
--           [ sig
--           , fun
--           ]
--  where
--   methodName' = mkName methodName
--
-- toConP :: [Name] -> [Q Pat]
-- toConP conNames = do
--   map
--     toConP'
--     conNames
--  where
--   toConP' conName = do
--     ary <- reify conName >>= arity
--     conP conName (replicate ary (newName "n" >>= varP))
--
-- functionLevels :: Type -> Int
-- functionLevels = go 0
--  where
--   go :: Int -> Type -> Int
--   go n (AppT (AppT ArrowT _) rest) =
--     go (n + 1) rest
--   go n (ForallT _ _ rest) =
--     go n rest
--   go n _ =
--     n
-- getType :: Info -> Maybe Type
-- getType (ClassOpI _ t _) = Just t
-- getType (DataConI _ t _) = Just t
-- getType (VarI _ t _) = Just t
-- getType (TyVarI _ t) = Just t
-- getType _ = Nothing
--
-- arity :: Info -> Q Int
-- arity info =
--   maybe
--     (reportError "Unable to get arity of name" >> return 0)
--     (return . functionLevels)
--     (getType info)

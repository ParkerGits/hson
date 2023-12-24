{-# LANGUAGE OverloadedStrings #-}

module Native where
import           Control.Monad.Except
import           Control.Monad.Reader (asks)
import qualified Data.Map             as Map
import           Data.Scientific
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           HSONValue

mkMethod :: (HSONValue -> [HSONValue] -> Eval HSONValue) -> HSONValue
mkMethod f = Method (Func . f)

arrayMethods :: Map.Map T.Text HSONValue
arrayMethods = Map.fromList [("length", mkMethod hsonLength), ("at", mkMethod hsonAt)]

hsonLength :: HSONValue -> [HSONValue] -> Eval HSONValue
hsonLength this [] = do
  case this of
    Array arr  -> return $ Number $ fromIntegral $ V.length arr
    String str -> return $ Number $ fromIntegral $ T.length str
hsonLength _ args = throwError $ ArgumentCount 0 args

hsonAt :: HSONValue -> [HSONValue] -> Eval HSONValue
hsonAt this [Number n] =
  case floatingOrInteger n of
    Left float -> throwError $ UnexpectedType "integer" "floating"
    Right idx -> case this of
      Array arr -> case arr V.!? idx of
        Just v  -> return v
        Nothing -> return Null

showType :: HSONValue -> T.Text
showType (Lambda _ _) = "function"
showType (Array _)    = "array"
showType (Object _)   = "object"
showType (String _)   = "string"
showType (Number _)   = "number"
showType (Bool _)     = "true"
showType Null         = "null"

module JSONParser (decode) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as A
import Data.Aeson.Types (parseJSON)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified HSONValue as H

instance A.FromJSON H.HSONValue where
  parseJSON (A.Array v) = H.Array <$> mapM parseJSON v
  parseJSON (A.Object v) = H.Object <$> mapM parseJSON (A.toMapText v)
  parseJSON (A.String v) = return $ H.String v
  parseJSON (A.Number v) = return $ H.Number v
  parseJSON (A.Bool v) = return $ H.Bool v
  parseJSON A.Null = return H.Null

instance A.ToJSON H.HSONValue where
  toJSON (H.Array v) = A.Array $ V.map A.toJSON v
  toJSON (H.Object v) = A.Object $ A.fromMapText $ Map.map A.toJSON v
  toJSON (H.String v) = A.String v
  toJSON (H.Number v) = A.Number v
  toJSON (H.Bool v) = A.Bool v
  toJSON _ = A.Null

  toEncoding (H.Array v) = A.list A.toEncoding (V.toList v)
  toEncoding (H.Object v) = A.dict A.text A.toEncoding Map.foldrWithKey v
  toEncoding (H.String v) = A.text v
  toEncoding (H.Number v) = A.scientific v
  toEncoding (H.Bool v) = A.bool v
  toEncoding _ = A.null_

decode :: BL.ByteString -> Either String H.HSONValue
decode = A.eitherDecode

module JSONParser(decode) where
import qualified Data.Aeson           as A
import qualified Data.Aeson.KeyMap    as A
import           Data.Aeson.Types     (parseJSON)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map
import qualified HSONValue            as H

instance A.FromJSON H.HSONValue where
  parseJSON (A.Array v) = do
    transformed <- mapM parseJSON v
    return $ H.Array transformed
  parseJSON (A.Object v) =
    let map = A.toMapText v in do
        transformed <- mapM parseJSON map
        return $ H.Object transformed
  parseJSON (A.String v) = return $ H.String v
  parseJSON (A.Number v) = return $ H.Number v
  parseJSON (A.Bool v) = return $ H.Bool v
  parseJSON A.Null = return H.Null

decode :: BL.ByteString -> Either String H.HSONValue
decode = A.eitherDecode

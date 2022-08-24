module Issues.Lib.Model.Util (
   fromBsonUuid, lookup', toBsonUuid
) where

import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Text (Text)
import           Data.UUID (fromByteString, toByteString, UUID)
import           Data.Bson (Document, Val)
import qualified Data.Bson as Bson

toBsonUuid :: UUID -> Bson.UUID
toBsonUuid = Bson.UUID . toStrict . toByteString

fromBsonUuid :: Bson.UUID -> Either Text UUID
fromBsonUuid (Bson.UUID bs) =
  case (fromByteString . fromStrict) bs of
    Nothing -> Left "failed to convert UUID"
    Just x -> Right x

lookup' :: Val a => Text -> Document -> Either Text a
lookup' label doc = case Bson.lookup label doc of
  Nothing -> Left $ "failed to find field [" <> label <> "]"
  Just x  -> Right x


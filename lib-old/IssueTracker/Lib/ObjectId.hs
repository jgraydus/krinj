{-# OPTIONS_GHC -Wno-orphans #-}
module IssueTracker.Lib.ObjectId (
    ObjectId
) where

import Prelude hiding (String)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(String))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Bson hiding (String)
import Data.Hashable (Hashable(..))
import Data.Text (pack, Text, unpack)
import Database.SQLite.Simple.FromField (FromField(..))
import Database.SQLite.Simple.ToField (ToField(..))
import Servant.API (FromHttpApiData(..))

toText :: ObjectId -> Text
toText = pack . show

fromText :: Text -> ObjectId
fromText = read . unpack 

instance ToJSON ObjectId where
  toJSON = String . toText

instance FromJSON ObjectId where
  parseJSON (String txt) = pure (fromText txt)
  parseJSON invalid = prependFailure "parsing ObjectId failed, " (typeMismatch "String" invalid)

instance FromHttpApiData ObjectId where
  parseUrlPiece = pure . fromText

instance Hashable ObjectId where
  hashWithSalt s (Oid v1 v2) = s `hashWithSalt` v1 `hashWithSalt` v2

instance FromField ObjectId where
  fromField = (fromText <$>) . fromField

instance ToField ObjectId where
  toField = toField . toText


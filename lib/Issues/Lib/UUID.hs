{-# OPTIONS_GHC -Wno-orphans #-}
module Issues.Lib.UUID (
  
) where

import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.UUID (fromText, nil, toText, UUID)
import           Database.SQLite.Simple.FromField (FromField(..))
import           Database.SQLite.Simple.ToField (ToField(..))

fromTextDefaultToNil :: Text -> UUID
fromTextDefaultToNil = fromMaybe nil . fromText

instance FromField UUID where
  fromField = (fmap fromTextDefaultToNil) . fromField

instance ToField UUID where
  toField = toField . toText


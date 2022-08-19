module Config where

import Data.Aeson
import Data.Char (toLower)
import GHC.Generics (Generic)

removeRecordPrefix :: String -> String -> String
removeRecordPrefix prefix = fmap toLower . drop (length prefix)

data MongoConfig = MongoConfig
  { _mongoHost :: String
  , _mongoPort :: Int
  } deriving (Generic, Show)

instance FromJSON MongoConfig where
  parseJSON = genericParseJSON
    defaultOptions { fieldLabelModifier = removeRecordPrefix "_mongo" }

data HttpConfig = HttpConfig
  { _httpConfigHost :: String
  , _httpConfigPort :: Int
  } deriving (Generic, Show)

instance FromJSON HttpConfig where
  parseJSON = genericParseJSON
    defaultOptions { fieldLabelModifier = removeRecordPrefix "_httpConfig" }

data LogLevel = TRACE | DEBUG | INFO | WARN | ERROR
  deriving (Generic, Show)

instance FromJSON LogLevel

data ApplicationConfig = ApplicationConfig
  { _applicationConfigMongo :: MongoConfig
  , _applicationConfigHttp :: HttpConfig
  , _applicationConfigLogLevel :: LogLevel
  } deriving (Generic, Show)

instance FromJSON ApplicationConfig where
  parseJSON = genericParseJSON
    defaultOptions { fieldLabelModifier = removeRecordPrefix "_applicationConfig" }


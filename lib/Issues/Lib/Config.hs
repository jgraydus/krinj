module Issues.Lib.Config where

import Data.Aeson
import Data.Char (toLower)
import GHC.Generics (Generic)

removeRecordPrefix :: String -> String -> String
removeRecordPrefix prefix = fmap toLower . drop (length prefix)

type HostString = String
type PortNumber = Int

data MongoConfig = MongoConfig
  { _mongoHost :: HostString
  , _mongoPort :: PortNumber
  } deriving (Generic, Show)

instance FromJSON MongoConfig where
  parseJSON = genericParseJSON
    defaultOptions { fieldLabelModifier = removeRecordPrefix "_mongo" }

data HttpConfig = HttpConfig
  { _httpConfigHost :: HostString
  , _httpConfigPort :: PortNumber
  } deriving (Generic, Show)

instance FromJSON HttpConfig where
  parseJSON = genericParseJSON
    defaultOptions { fieldLabelModifier = removeRecordPrefix "_httpConfig" }

data LogLevel = TRACE | DEBUG | INFO | WARN | ERROR
  deriving (Generic, Show)

instance FromJSON LogLevel

type JwtKey = String

data ApplicationConfig = ApplicationConfig
  { _applicationConfigMongo :: MongoConfig
  , _applicationConfigHttp :: HttpConfig
  , _applicationConfigLogLevel :: LogLevel
  , _applicationConfigJwtKey :: JwtKey
  } deriving (Generic, Show)

instance FromJSON ApplicationConfig where
  parseJSON = genericParseJSON
    defaultOptions { fieldLabelModifier = removeRecordPrefix "_applicationConfig" }


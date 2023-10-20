module Krinj.Config where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import JsonConfig qualified
import Krinj.Logger (LogLevel(..))

newtype HostName = HostName Text
  deriving stock (Generic)
  deriving newtype (Eq, FromJSON, Ord, Show, ToJSON)

newtype PortNumber = PortNumber Int
  deriving stock (Generic)
  deriving newtype (Eq, FromJSON, Ord, Show, ToJSON)

newtype Protocol = Protocol Text
  deriving stock (Generic)
  deriving newtype (Eq, FromJSON, Ord, Show, ToJSON)

newtype JwtKey = JwtKey Text
  deriving stock (Generic)
  deriving newtype (Eq, FromJSON, Ord, Show, ToJSON)

data HttpServerConfig = HttpServerConfig
  { protocol :: Protocol
  , host :: HostName
  , port :: PortNumber
  , jwtKey :: JwtKey
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data ApplicationConfig = ApplicationConfig
  { httpServerConfig :: HttpServerConfig
  , logLevel :: LogLevel
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- | parse the config files found in the given directory
readConfig :: FilePath -> String -> IO (Either String ApplicationConfig)
readConfig = JsonConfig.readConfig


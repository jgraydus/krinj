module Krinj.Config (
    ApplicationConfig(..),
    HttpServerConfig(..),
    readConfig,

    module Krinj.Config.DatabaseConfig,
    module Krinj.Config.Newtypes,
) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import JsonConfig qualified
import Krinj.Config.DatabaseConfig
import Krinj.Config.Newtypes
import Krinj.Logger.Types (LogLevel(..))
import Krinj.UserService.Types (Salt)

data HttpServerConfig = HttpServerConfig
  { host :: HostName
  , jwtKey :: JwtKey
  , port :: PortNumber
  , protocol :: Protocol
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data ApplicationConfig = ApplicationConfig
  { databaseConfig :: DatabaseConfig
  , httpServerConfig :: HttpServerConfig
  , logLevel :: LogLevel
  , salt :: Salt
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- | parse the config files found in the given directory
readConfig :: FilePath -> String -> IO (Either String ApplicationConfig)
readConfig = JsonConfig.readConfig


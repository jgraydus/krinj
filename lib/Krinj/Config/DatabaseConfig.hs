module Krinj.Config.DatabaseConfig (
    DatabaseConfig(..), makeConnectInfo
) where

import Data.Aeson (FromJSON)
import Data.Coerce (coerce)
import Data.Text (Text, unpack)
import Database.PostgreSQL.Simple (ConnectInfo(..), defaultConnectInfo)
import GHC.Generics (Generic)
import Krinj.Config.Newtypes (Password(..), PortNumber(..))

data DatabaseConfig = DatabaseConfig
  { database :: Text
  , password :: Password
  , port :: PortNumber
  , user :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

makeConnectInfo :: DatabaseConfig -> IO ConnectInfo
makeConnectInfo DatabaseConfig {..} = pure $ defaultConnectInfo
  { connectPort = fromIntegral . (coerce @PortNumber @Int) $ port
  , connectUser = unpack user
  , connectPassword = unpack . coerce $ password
  , connectDatabase = unpack database
  }


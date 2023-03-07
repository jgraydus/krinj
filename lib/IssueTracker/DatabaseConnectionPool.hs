module IssueTracker.DatabaseConnectionPool (
    create, DatabaseConnectionPoolConfig(..), defaultConfig
) where 

import Control.Concurrent (getNumCapabilities)
import Data.Pool (defaultPoolConfig, newPool, Pool, setNumStripes)
import Database.PostgreSQL.Simple (close, connect, ConnectInfo, Connection)

data DatabaseConnectionPoolConfig = 
  DatabaseConnectionPoolConfig {
    numStripes :: Maybe Int -- a stripe is an independently managed sub pool
  , poolCacheTTL :: Double -- time in seconds to keep an unused connection alive 
  , poolMaxResources :: Int
  } deriving (Show)

defaultConfig :: IO DatabaseConnectionPoolConfig
defaultConfig = do
  n <- getNumCapabilities
  let numStripes = Just n
      poolCacheTTL = 10 
      poolMaxResources = n * 4
  pure DatabaseConnectionPoolConfig {..}

create :: DatabaseConnectionPoolConfig -> ConnectInfo -> IO (Pool Connection)
create config connectInfo = newPool poolConfig
  where
    DatabaseConnectionPoolConfig {..} = config
    poolConfig = setNumStripes numStripes $
      defaultPoolConfig createResource freeResource poolCacheTTL poolMaxResources 
    -- PoolConfig fields:
    createResource = connect connectInfo
    freeResource = close


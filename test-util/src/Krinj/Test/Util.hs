module Krinj.Test.Util (
   getConnectInfo,
   withTestMinio,
   withTestDatabase,
   withTestRedis,
) where

-- import Data.String (fromString)
-- import Data.Text (unpack)
import Database.PostgreSQL.Simple qualified as P
-- import Database.Redis qualified as R
import JsonConfig
import Krinj.Config (ApplicationConfig(..), makePostgresqlConnectInfo)
-- import Network.Minio qualified as M
import Krinj.Test.Util.Minio (withTestMinio)
import Krinj.Test.Util.PostgreSQL (withTestDatabase)
import Krinj.Test.Util.Redis (withTestRedis)

--getConnectInfo :: IO (P.ConnectInfo, R.ConnectInfo, M.ConnectInfo)
getConnectInfo :: IO P.ConnectInfo
getConnectInfo = do
  Right ApplicationConfig {..} <- readConfig "../config" "localhost"
  let p = makePostgresqlConnectInfo databaseConfig
      -- RedisConfig {..} = _applicationConfigRedis
      -- MinioConfig {..} = _applicationConfigMinio
      -- r = R.defaultConnectInfo
      --     { R.connectHost     = _redisConfigHost
      --     , R.connectPort     = R.PortNumber $ fromInteger _redisConfigPort
      --     }
      -- credentials = M.CredentialValue
      --                   (fromString . unpack $ _minioConfigAccessKey)
      --                   (fromString . unpack $ _minioConfigSecretKey)
      --                   Nothing
      -- m :: M.ConnectInfo = M.setCreds credentials (fromString . unpack $ _minioConfigHost)
  -- pure (p, r, m)
  pure p


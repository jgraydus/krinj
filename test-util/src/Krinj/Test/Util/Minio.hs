module Krinj.Test.Util.Minio (
    withTestMinio
) where

import Data.Text (Text)
import Data.UUID (toText)
import Data.UUID.V4 qualified as UUID
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Minio ( ConnectInfo, isConnectInfoSecure, makeBucket, MinioConn
                     , mkMinioConn, removeBucket, runMinioWith )

-- | provide an action with a minio connection and randomly generated bucket name.
--   after the action completes, the bucket is deleted
withTestMinio
  :: ConnectInfo
  -> ((MinioConn, Text) -> IO a)
  -> IO a
withTestMinio connectInfo action = do
  manager <- if isConnectInfoSecure connectInfo
             then newTlsManager 
             else newManager defaultManagerSettings
  conn <- mkMinioConn connectInfo manager
  uuid <- UUID.nextRandom
  let bucket = "test-bucket-" <> (toText uuid)
  _ <- runMinioWith conn $ makeBucket bucket Nothing
  result <- action (conn, bucket)
  _ <- runMinioWith conn $ removeBucket bucket
  pure result


module Krinj.Test.Util.Redis where

import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket)
import Database.Redis ( Connection
                      , ConnectInfo(..)
                      , disconnect
                      , flushdb
                      , runRedis
                      , withCheckedConnect)

testDatabaseNumber :: Integer
testDatabaseNumber = 15

withTestRedis
  :: ConnectInfo
  -> (Connection -> IO a)
  -> IO a
withTestRedis connectInfo action = withCheckedConnect connectInfo' $ \conn -> do
  result <- action conn
  _ <- runRedis conn flushdb
  pure result
  where connectInfo' = connectInfo
                       { connectDatabase = testDatabaseNumber
                       , connectMaxConnections = 1
                       }


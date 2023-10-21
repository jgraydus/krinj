module Krinj.Test.Util.PostgreSQL where

import Control.Exception (bracket)
import Data.Pool (defaultPoolConfig, destroyAllResources, newPool, Pool, withResource)
import Data.Text (Text, pack)
import Data.Word (Word16)
import Database.PostgreSQL.Simple (close, connect, ConnectInfo(..), Connection, execute, execute_)
import Database.PostgreSQL.Simple.Types (Identifier(..))
import Database.PostgreSQL.Simple.Migration ( defaultOptions
                                            , MigrationCommand(..)
                                            , runMigration
                                            , runMigrations)
import System.Random

migrationDirs :: [FilePath]
migrationDirs = [
    "../user-service/migrations"
  , "../entity-service/migrations"
  ]

-- | create a new empty database, create all the necessary tables,
--   run the given action, and then delete the database
withTestDatabase
  :: ConnectInfo
  -> (Pool Connection -> IO a)
  -> IO a
withTestDatabase connectInfo action = do
  -- create a name for the database
  n :: Word16 <- randomIO
  let databaseName :: String = "testdb_" ++ show n

  let createDatabase = do 
        conn <- connect connectInfo
        execute conn "create database ?;" [Identifier $ pack databaseName]
        return conn

  let dropDatabase conn = do
        execute conn "drop database ?;" [Identifier $ pack databaseName]
        close conn

  let work _ = do
        -- create a pool of connections to the new database
        let createPool = do
              -- PoolConfig fields
              let createResource = connect connectInfo { connectDatabase = databaseName}
                  freeResource = close
                  poolCacheTTL = 2.0 
                  poolMaxResources = 100

              newPool $ defaultPoolConfig createResource freeResource poolCacheTTL poolMaxResources

        let run pool = withResource pool $ \conn -> do
              -- run all the migrations for the database
              _ <- runMigration conn defaultOptions MigrationInitialization
              let commands = MigrationDirectory <$> migrationDirs
              _ <- runMigrations conn defaultOptions commands

              -- run the given action in the new database
              action pool

        -- if we don't close out all open connections to the database, we won't be able to drop it
        bracket createPool destroyAllResources run

  bracket createDatabase dropDatabase work


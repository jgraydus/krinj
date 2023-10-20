module Main where

import CommandLineArgs
import Data.Int (Int64)
import Database.PostgreSQL.Simple
  (connect, ConnectInfo(..), execute, Only(..), query)
import Database.PostgreSQL.Simple.Types (Identifier(..))
import Database.PostgreSQL.Simple.Migration
  (defaultOptions, MigrationCommand(..), runMigration)
import Data.Text (pack)
import Krinj.Config (ApplicationConfig(..), makeConnectInfo, readConfig)
import System.Exit (exitFailure, exitSuccess)

deleteDatabase :: ConnectInfo -> IO ()
deleteDatabase connectInfo = do
  let databaseName = connectDatabase connectInfo
  conn <- connect (connectInfo { connectDatabase = "postgres" })
  -- first determine if the database exists
  [Only count] :: [Only Int64] <- query conn
     "select count(*) from pg_catalog.pg_database where datname = ?;"
    [databaseName]
  if count == 0 then
    putStrLn "database does not exist - skipping"
  else do
    -- kill any connections to the database, otherwise postgres will not let us delete it
    _ :: [Only Int] <- query conn
       "select pg_terminate_backend (pid) \
        \ from pg_stat_activity \
        \ where datname = ?;"
        [databaseName]
    _ <- execute conn "drop database ?;" [Identifier $ pack databaseName]
    pure ()

createDatabase :: ConnectInfo -> IO ()
createDatabase connectInfo = do
  let databaseName = Identifier (pack (connectDatabase connectInfo))
  conn <- connect (connectInfo { connectDatabase = "postgres" })
  _ <- execute conn "create database ?;" [databaseName]
  pure ()

main :: IO ()
main = do
  putStrLn "---------------------------------------------"
  putStrLn "--------------- DATABASE INIT ---------------"
  putStrLn "---------------------------------------------"
  putStrLn ""

  CommandLineArgs {..} <- parseCommandLineArgs
  putStrLn $ "environment:      " <> show _executionEnv
  putStrLn $ "config directory: " <> _configFileDir
  putStrLn ""
  configOrErr <- readConfig _configFileDir (show _executionEnv)

  case configOrErr of
    Left err -> do
      putStrLn "*********************** ERROR *************************"
      print err
      exitFailure

    Right _config -> do
      let ApplicationConfig {..} = _config
      connectInfo <- makeConnectInfo databaseConfig
      print $ connectInfo { connectPassword = "*********" }
      putStrLn ""

      putStrLn "WARNING: this will delete any existing data!"
      putStrLn "are you sure you want to continue? y/n"
      answer <- getChar

      if answer == 'y' || answer == 'Y' then do
        putStrLn "********* Initializing database *********"

        putStrLn "> deleting existing data <"
        _ <- deleteDatabase connectInfo
        putStrLn "done"

        putStrLn "> creating database <"
        _ <- createDatabase connectInfo
        putStrLn "done"

        putStrLn "> creating migration metadata table <"
        conn <- connect connectInfo
        _ <- runMigration conn defaultOptions MigrationInitialization
        putStrLn "done"

        exitSuccess

      else do
        putStrLn "ABORTING"
        exitSuccess


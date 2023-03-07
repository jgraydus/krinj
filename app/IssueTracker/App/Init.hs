module IssueTracker.App.Init where

import Data.Int (Int64)
import Database.PostgreSQL.Simple
  (connect, ConnectInfo(..), defaultConnectInfo, execute, Only(..), query)
import Database.PostgreSQL.Simple.Types (Identifier(..))
import Database.PostgreSQL.Simple.Migration
         (defaultOptions, MigrationCommand(..), runMigration, runMigrations, ScriptName)
import Data.Text (pack)
import System.Exit (exitSuccess)

initScripts :: [(ScriptName, FilePath)]
initScripts = [("entity-service init", "./entity-service/model.sql")]

getConnectInfo :: IO ConnectInfo
getConnectInfo = pure $ defaultConnectInfo
  { connectPort = 15432
  , connectUser = "postgres"
  , connectPassword = "password"
  , connectDatabase = "main"
  }

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
  putStrLn "--------------- DATABASE INIT ---------------"
  putStrLn "WARNING: this will delete any existing data!"
  putStrLn "are you sure you want to continue? y/n"
  answer <- getChar

  if answer == 'y' || answer == 'Y' then do
    putStrLn "*** Initializing database ***"

    connectInfo <- getConnectInfo
    print $ connectInfo { connectPassword = "*********" }

    putStrLn "> deleting existing data <"
    _ <- deleteDatabase connectInfo
    putStrLn "done"

    putStrLn "> creating database <"
    _ <- createDatabase connectInfo
    putStrLn "done"

    conn <- connect connectInfo

    putStrLn "> creating migration metadata table <"
    _ <- runMigration conn defaultOptions MigrationInitialization
    putStrLn "done"

    putStrLn "> running intialization scripts <"
    let commands = uncurry MigrationFile <$> initScripts
    _ <- runMigrations conn defaultOptions commands
    putStrLn "done"

    exitSuccess

  else do
    putStrLn "ABORTING"
    exitSuccess


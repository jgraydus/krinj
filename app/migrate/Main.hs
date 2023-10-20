module Main where

import CommandLineArgs
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import IssueTracker.Config (readConfig)
import System.Exit (exitFailure)

connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo
  { connectPort = 15432
  , connectUser = "postgres"
  , connectPassword = "password"
  , connectDatabase = "main"
  }

migrationDirs :: [FilePath]
migrationDirs = [
    "./entity-service/migrations"
  ]

logger :: Either Text Text -> IO ()
logger = \case
  Left err -> Text.putStrLn $ "[ERROR] " <> err
  Right msg -> Text.putStrLn $ "[OK] " <> msg

main :: IO ()
main = do
  putStrLn "*********************************************************"
  putStrLn "*                  running migrations                   *"
  putStrLn "*********************************************************"
  CommandLineArgs {..} <- parseCommandLineArgs
  putStrLn $ "environment:      " <> show _executionEnv
  putStrLn $ "config directory: " <> _configFileDir
  configOrErr <- readConfig _configFileDir (show _executionEnv)

  case configOrErr of
    Left err -> do
      putStrLn "*********************** ERROR *************************"
      print err
      exitFailure

    Right _config -> do
      print $ connectInfo { connectPassword = "*********" }
      conn <- connect connectInfo

      let options = defaultOptions { optLogWriter = logger, optVerbose = Verbose }
          commands = MigrationDirectory <$> migrationDirs
      _ <- runMigrations conn options commands

      pure ()


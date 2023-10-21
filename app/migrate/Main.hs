module Main where

import CommandLineArgs
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import Krinj.Config (ApplicationConfig(..), makePostgresqlConnectInfo, readConfig)
import System.Exit (exitFailure)

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
      let ApplicationConfig {..} = _config
          connectInfo = makePostgresqlConnectInfo databaseConfig
      print $ connectInfo { connectPassword = "*********" }
      conn <- connect connectInfo

      let options = defaultOptions { optLogWriter = logger, optVerbose = Verbose }
          commands = MigrationDirectory <$> migrationDirs
      _ <- runMigrations conn options commands

      pure ()


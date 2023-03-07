module IssueTracker.App.Web where

import Data.Coerce (coerce)
import Database.PostgreSQL.Simple (ConnectInfo(..), defaultConnectInfo)
import IssueTracker.Logger (LogLevel(INFO), newLogger, toLogStr)
import IssueTracker.Config (ApplicationConfig(..), HttpServerConfig(..), PortNumber(..), readConfig)
import IssueTracker.DatabaseConnectionPool
import IssueTracker.Options (env, Options(..), parseOptions)
import IssueTracker.Web.Application (app)
import Network.Wai.Handler.Warp qualified as Warp
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)

connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo { connectPort = 15432 }

main :: IO ()
main = do
  -- parse the command line options
  Options {..} <- parseOptions

  -- read the application configuration files
  config <- readConfig _optionsConfigDir (env _optionsMode)

  case config of
    Left errorMessage -> do
      putStrLn errorMessage
      exitWith (ExitFailure 1)

    Right applicationConfig@ApplicationConfig {..} -> do
      let HttpServerConfig {..} = httpServerConfig
      (logger, loggerCleanup) <- newLogger logLevel

      logger INFO (toLogStr (show applicationConfig))

      databaseConnectionPoolConfig <- defaultConfig
      databaseConnectionPool <- create databaseConnectionPoolConfig connectInfo

      logger INFO (toLogStr $ "server listening on port " <> show port)

      Warp.run (coerce port) (app databaseConnectionPool applicationConfig logger)

      _ <- loggerCleanup
      exitSuccess


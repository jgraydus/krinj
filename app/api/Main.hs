module Main where

import Data.Coerce (coerce)
import Krinj.Logger (LogLevel(INFO), newLogger, toLogStr)
import Krinj.Config (ApplicationConfig(..), HttpServerConfig(..), makePostgresqlConnectInfo, PortNumber(..), readConfig)
import Krinj.DatabaseConnectionPool
import Krinj.Options (env, Options(..), parseOptions)
import Krinj.Web.Application (app)
import Network.Wai.Handler.Warp qualified as Warp
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)

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
      let connectInfo = makePostgresqlConnectInfo databaseConfig
      databaseConnectionPool <- create databaseConnectionPoolConfig connectInfo

      logger INFO (toLogStr $ "server listening on port " <> show port)

      Warp.run (coerce port) (app databaseConnectionPool applicationConfig logger)

      _ <- loggerCleanup
      exitSuccess


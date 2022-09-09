module IssueTracker.App.Web where

import qualified Network.Wai.Handler.Warp as Warp
import           System.Exit (ExitCode(..), exitWith)

import qualified IssueTracker.Lib.Language.Interpreter.Sqlite as Sqlite
import qualified IssueTracker.Lib.Language.Interpreter.InMemory as InMemory
import           IssueTracker.Lib.Logger (LogLevel(INFO), newLogger, toLogStr)
import           IssueTracker.Lib.Config (ApplicationConfig(..), HttpConfig(..), Implementation(..), readConfig,
                                          SqliteConfig(..))
import           IssueTracker.Lib.Web.Application
import           IssueTracker.Lib.Web.Options

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
      
    Right c@ApplicationConfig {..} -> do
      let HttpConfig {..} = _applicationConfigHttp
      (logger, loggerCleanup) <- newLogger _applicationConfigLogLevel

      logger INFO (toLogStr (show c)) 

      logger INFO (toLogStr $ "server listening on port " <> show _httpConfigPort)

      let program rt = Warp.run _httpConfigPort (app c rt logger)

      case _httpConfigImplementation of
        InMemory -> InMemory.withRunTime program
        Sqlite SqliteConfig {..} -> Sqlite.withRunTime filePath program
        _ -> error "unsupported implementation"

      _ <- loggerCleanup
      exitWith ExitSuccess


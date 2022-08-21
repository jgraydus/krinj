module Issues.App.Web where

import qualified Network.Wai.Handler.Warp as Warp
import           System.Exit (ExitCode(..), exitWith)

import           Issues.Lib.Language.Interpreter.InMemory (makeRunTime)
import           Issues.Lib.Logger (newLogger)
import           Issues.Lib.Config (ApplicationConfig(..), HttpConfig(..), readConfig)
import           Issues.Lib.Web.Application
import           Issues.Lib.Web.Options

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
      
    Right ApplicationConfig {..} -> do
      let HttpConfig {..} = _applicationConfigHttp
      putStrLn $ "server listening on port " <> (show _httpConfigPort)
      (logger, loggerCleanup) <- newLogger _applicationConfigLogLevel
      rt <- makeRunTime logger
      Warp.run _httpConfigPort (app _applicationConfigJwtKey rt)
      _ <- loggerCleanup
      exitWith ExitSuccess


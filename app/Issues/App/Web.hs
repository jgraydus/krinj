module Issues.App.Web where

import           JsonConfig
import qualified Network.Wai.Handler.Warp as Warp
import qualified StmContainers.Map as StmMap
import           System.Exit (ExitCode(..), exitWith)

import           Issues.Lib.Language
import           Issues.Lib.Config
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
      db <- DB <$> StmMap.newIO <*> StmMap.newIO <*> StmMap.newIO
      Warp.run _httpConfigPort (app _applicationConfigJwtKey db)
      exitWith ExitSuccess


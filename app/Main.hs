module Main where

import           Application
import           Args
import           Config
import           Data.Text (Text)
import           GenerateJsBindings
import           JsonConfig
import qualified Network.Wai.Handler.Warp as Warp
import           System.Exit (ExitCode(..), exitWith)

env :: Mode -> String
env DEV  = "localhost"
env PROD = "prod"

main :: IO ()
main = do
  -- parse the command line options
  Options {..} <- parseOptions

  case _optionsCommand of

    -- start the http server
    HttpServer -> do
      -- read the application configuration files
      config <- readConfig _optionsConfigDir (env _optionsMode)

      case config of
        Left errorMessage -> do
          putStrLn errorMessage
          exitWith (ExitFailure 1)
          
        Right ApplicationConfig {..} -> do
          let HttpConfig {..} = _applicationConfigHttp
          putStrLn $ "server listening on port " <> (show _httpConfigPort)
          Warp.run _httpConfigPort app
          exitWith ExitSuccess

    -- generate javascript code to make http requests to the end points
    GenerateJsBindings -> do
      putStrLn "generating javascript bindings for http endpoints"
      writeJSCode
      putStrLn "DONE"
      exitWith ExitSuccess


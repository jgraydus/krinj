module Main where

import           Application
import           Args
import           Auth
import           Config
import           Data.Text (Text, unpack)
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.UUID.V4 as UUID
import           GenerateJsBindings
import           JsonConfig
import           Model
import qualified Network.Wai.Handler.Warp as Warp
import qualified StmContainers.Map as StmMap
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
          db <- StmMap.newIO
          Warp.run _httpConfigPort (app _applicationConfigJwtKey db)
          exitWith ExitSuccess

    -- generate javascript code to make http requests to the end points
    GenerateJsBindings -> do
      putStrLn "generating javascript bindings for http endpoints"
      writeJSCode
      putStrLn "DONE"
      exitWith ExitSuccess

    -- generate a user with a random uuid and print out an auth token for that user
    MakeAuthTokenForTesting -> do
      config <- readConfig _optionsConfigDir (env _optionsMode)

      case config of
        Left errorMessage -> do
          putStrLn errorMessage
          exitWith (ExitFailure 1)

        Right ApplicationConfig {..} -> do 
          userId <- UUID.nextRandom
          let user = User userId
          let token = makeAuthToken _applicationConfigJwtKey user
          putStrLn $ (unpack . decodeUtf8) token


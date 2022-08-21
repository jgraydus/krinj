module Issues.App.MakeAuthToken where

import           JsonConfig (readConfig)
import           System.Exit (ExitCode(..), exitWith)

import           Issues.Lib.Auth (makeAuthToken)
import           Issues.Lib.Config (ApplicationConfig(..))
import           Issues.Lib.Model (makeUserForTest)
import           Issues.Lib.Web.Options (env, Options(..), parseOptions)

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
      user <- makeUserForTest 
      let token = makeAuthToken _applicationConfigJwtKey user
      print token
      exitWith ExitSuccess


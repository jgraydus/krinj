module IssueTracker.App.MakeAuthToken where


import IssueTracker.Lib.Auth (makeAuthToken)
import IssueTracker.Lib.Config (ApplicationConfig(..), readConfig)
import IssueTracker.Lib.Model (makeUserForTest)
import IssueTracker.Lib.Web.Options (env, Options(..), parseOptions)
import System.Exit (ExitCode(..), exitSuccess, exitWith)

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
      exitSuccess


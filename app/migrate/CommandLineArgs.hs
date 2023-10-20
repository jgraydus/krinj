module CommandLineArgs (
    CommandLineArgs(..), ExecutionEnv, parseCommandLineArgs
) where 

import Options.Applicative

data ExecutionEnv = LOCALHOST | PRODUCTION deriving Show

data CommandLineArgs = 
  CommandLineArgs {
    _executionEnv :: ExecutionEnv
  , _configFileDir :: String
  } deriving Show

readCommandLineArgs :: ReadM ExecutionEnv
readCommandLineArgs = eitherReader $ \case 
  "localhost"  -> Right LOCALHOST
  "production" -> Right PRODUCTION
  _            -> Left "valid values: [localhost, production]"

parser :: Parser CommandLineArgs
parser = CommandLineArgs
  <$> option readCommandLineArgs 
      (  long "execution-environment" 
      <> short 'e' 
      <> value LOCALHOST
      <> metavar "ENVIRONMENT")
  <*> strOption 
      (  long "config-file-dir"
      <> value "./config"
      <> metavar "DIRECTORY" )

parseCommandLineArgs :: IO CommandLineArgs
parseCommandLineArgs = execParser (info parser fullDesc)


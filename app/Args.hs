module Args (
    Command(..),
    Mode(..),
    Options(..),
    parseOptions
) where

import Options.Applicative

data Mode = DEV | PROD
  deriving (Read, Show)

data Command = HttpServer | GenerateJsBindings | MakeAuthTokenForTesting
  deriving (Read, Show)

data Options = Options
  { _optionsMode :: Mode
  , _optionsCommand :: Command
  , _optionsConfigDir :: FilePath
  } deriving (Show)

envParser :: Parser Mode
envParser = option auto
  (  long "mode"
  <> metavar "ENVIRONMENT" 
  <> value DEV
  <> help "execution mode: DEV or PROD" )

commandParser :: Parser Command
commandParser = option auto
  (  long "command"
  <> metavar "COMMAND"
  <> value HttpServer
  <> help "command to execute: HttpServer, GenerateJsBindings, or MakeAuthTokenForTesting" )

configDirParser :: Parser FilePath
configDirParser = strOption
  (  long "config-directory"
  <> metavar "CONFIG DIRECTORY"
  <> value "./config"
  <> help "directory containing application configuration files" )

parser :: Parser Options
parser = Options <$> envParser <*> commandParser <*> configDirParser

parseOptions :: IO Options
parseOptions = execParser (info parser mempty)


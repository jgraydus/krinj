module IssueTracker.Lib.Web.Options (
    env,
    Mode(..),
    Options(..),
    parseOptions
) where

import Options.Applicative (auto, execParser, help, info, long, metavar,
                            option, Parser, strOption, value)

data Mode = DEV | PROD
  deriving (Read, Show)

env :: Mode -> String
env DEV  = "localhost"
env PROD = "prod"

data Options = Options
  { _optionsMode :: Mode
  , _optionsConfigDir :: FilePath
  } deriving (Show)

envParser :: Parser Mode
envParser = option auto
  (  long "mode"
  <> metavar "ENVIRONMENT" 
  <> value DEV
  <> help "execution mode: DEV or PROD" )

configDirParser :: Parser FilePath
configDirParser = strOption
  (  long "config-directory"
  <> metavar "CONFIG DIRECTORY"
  <> value "./config"
  <> help "directory containing application configuration files" )

parser :: Parser Options
parser = Options <$> envParser <*> configDirParser

parseOptions :: IO Options
parseOptions = execParser (info parser mempty)


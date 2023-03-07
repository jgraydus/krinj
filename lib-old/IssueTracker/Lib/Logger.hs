module IssueTracker.Lib.Logger (
    Logger,
    LogLevel(..),
    newLogger,
    toLogStr
) where

import Control.Monad (when)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Prelude hiding (log)
import System.Log.FastLogger (LogType'(LogStdout), LogStr, newTimeCache,
                              newTimedFastLogger, simpleTimeFormat', ToLogStr(..))

data LogLevel = TRACE | DEBUG | INFO | WARN | ERROR
  deriving (Eq, Generic, Ord, Show)

instance FromJSON LogLevel

instance ToLogStr LogLevel where
  toLogStr = toLogStr . show

type Logger = LogLevel -> LogStr -> IO ()
type LoggerCleanup = IO ()

-- this character string tells the terminal to render the text in green
green :: LogStr
green = toLogStr ("\x1b[38;2;78;154;6m" :: String)

newLogger :: LogLevel -> IO (Logger, LoggerCleanup)
newLogger threshold = do
  timeCache <- newTimeCache simpleTimeFormat'
  (log, cleanup) <- newTimedFastLogger timeCache (LogStdout 4096)
  let logger logLevel msg = when (logLevel >= threshold) $
        log (\time -> green <> "[" <> toLogStr logLevel <> "][" <> toLogStr time <> "]" <> msg <> "\n")
  return (logger, cleanup)


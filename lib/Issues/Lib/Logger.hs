module Issues.Lib.Logger where

import Prelude hiding (log)

import Control.Monad (when)
import GHC.Generics (Generic)
import System.Log.FastLogger

data LogLevel = TRACE | DEBUG | INFO | WARN | ERROR
  deriving (Eq, Generic, Ord, Show)

instance ToLogStr LogLevel where
  toLogStr = toLogStr . show

type Logger = LogLevel -> LogStr -> IO ()
type LoggerCleanup = IO ()

newLogger :: LogLevel -> IO (Logger, LoggerCleanup)
newLogger threshold = do
  timeCache <- newTimeCache simpleTimeFormat'
  (log, cleanup) <- newTimedFastLogger timeCache (LogStdout 4096)
  let logger logLevel msg = when (logLevel >= threshold) $
        log (\time -> "[" <> toLogStr logLevel <> "][" <> toLogStr time <> "] " <> msg <> "\n")
  return (logger, cleanup)


{-# LANGUAGE UndecidableInstances #-}
module Krinj.Logger (
    Logger,
    LogLevel(..),
    LogStr,
    newLogger,
    toLogStr,
    MonadLogger(..),
    logTrace, logDebug, logInfo, logWarn, logError
) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (asks, MonadReader)
import GHC.Records (getField, HasField)
import Krinj.Logger.Types
import Prelude hiding (log)
import System.Log.FastLogger (LogType'(LogStdout), LogStr, newTimeCache,
                              newTimedFastLogger, simpleTimeFormat', ToLogStr(..))

-- this character string tells the terminal to render the text in green
green :: LogStr
green = toLogStr ("\x1b[38;2;78;154;6m" :: String)

newLogger :: LogLevel -> IO (Logger, LoggerCleanup)
newLogger threshold = do
  timeCache <- newTimeCache simpleTimeFormat'
  (log', cleanup) <- newTimedFastLogger timeCache (LogStdout 4096)
  let logger logLevel msg = when (logLevel >= threshold) $
        log' (\time -> green <> "[" <> toLogStr logLevel <> "][" <> toLogStr time <> "]" <> msg <> "\n")
  return (logger, cleanup)

class Monad m => MonadLogger m where
  log :: LogLevel -> LogStr -> m ()

logTrace :: MonadLogger m => LogStr -> m ()
logTrace = log TRACE

logDebug :: MonadLogger m => LogStr -> m ()
logDebug = log DEBUG

logInfo :: MonadLogger m => LogStr -> m ()
logInfo = log INFO

logWarn :: MonadLogger m => LogStr -> m ()
logWarn = log WARN

logError :: MonadLogger m => LogStr -> m ()
logError = log ERROR

instance (Monad m, MonadIO m, MonadReader r m, HasField "logger" r Logger) => MonadLogger m where
  log :: LogLevel -> LogStr -> m ()
  log logLevel logStr = do
    logger <- asks (getField @"logger")
    liftIO $ logger logLevel logStr


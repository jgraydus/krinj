module Krinj.Logger.Types where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import System.Log.FastLogger (LogStr, ToLogStr(..))

data LogLevel = TRACE | DEBUG | INFO | WARN | ERROR
  deriving (Eq, Generic, Ord, Show)

instance FromJSON LogLevel

instance ToLogStr LogLevel where
  toLogStr = toLogStr . show

type Logger = LogLevel -> LogStr -> IO ()
type LoggerCleanup = IO ()


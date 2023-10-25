module Krinj.Web.Application (
    app
) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Proxy (Proxy(..))
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Krinj.Config (ApplicationConfig(..))
import Krinj.Logger (Logger, LogLevel(INFO), LogStr, toLogStr)
import Krinj.Web.RequestContext
import Krinj.Web.Routes
import Network.Wai (Application, rawQueryString, rawPathInfo, Request, requestMethod)
import Servant (Context(..), Handler(..), ServerError, serveWithContextT)
import System.Clock (Clock(Monotonic), diffTimeSpec, getTime, TimeSpec, toNanoSecs)

timeDiff :: TimeSpec -> TimeSpec -> Double
timeDiff start end = milliSecs
  where
    diff = diffTimeSpec start end
    nanoSecs :: Double = fromIntegral $ toNanoSecs diff
    milliSecs = nanoSecs / 1000000.0

withLoggedDuration :: Logger -> IO a -> IO a
withLoggedDuration logger action = do
  startTime <- getTime Monotonic
  result <- action
  endTime <- getTime Monotonic
  let diff = toLogStr $ timeDiff startTime endTime
  logger INFO ("request elapsed ms: " <> diff)
  pure result

-- add the request id to the logged message
withReqId :: Logger -> RequestId -> Logger
withReqId logger reqId level msg = logger level ("[" <> reqId' <> "]" <> msg)
  where reqId' = toLogStr (UUID.toASCIIBytes reqId)

pathLogStr :: Request -> LogStr
pathLogStr req =
  toLogStr (requestMethod req)
  <> " "
  <> toLogStr (rawPathInfo req)
  <> toLogStr (rawQueryString req)

p :: Proxy APIAndSite
p = Proxy

toHandler :: RequestContext -> ReaderT RequestContext (ExceptT ServerError IO) x -> Handler x
toHandler ctx m = Handler (runReaderT m ctx)

app :: Pool Connection -> ApplicationConfig -> Logger -> Application
app databaseConnectionPool applicationConfig l req res = do
  requestId <- UUID.nextRandom

  let logger = withReqId l requestId
      reqCxt = RequestContext {..}
  _ <- logger INFO (pathLogStr req)

  withLoggedDuration logger $
    serveWithContextT p (reqCxt :. EmptyContext) (toHandler reqCxt) apiAndSiteHandler req res


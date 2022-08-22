module Issues.Lib.Web.Application (
    app
) where

import           Data.UUID (toASCIIBytes, UUID)
import           Data.UUID.V4 (nextRandom)
import           Network.Wai (rawQueryString, rawPathInfo, Request, requestMethod)
import           Servant
import           System.Clock (Clock(Monotonic), diffTimeSpec, getTime, TimeSpec, toNanoSecs)
import           System.Log.FastLogger (LogStr)

import           Issues.Lib.Auth (authContext)
import           Issues.Lib.Config (JwtKey)
import           Issues.Lib.ContentTypes ()
import           Issues.Lib.Language.RunTime (AppHandler, runAppHandler, RunTime)
import           Issues.Lib.Logger (Logger, LogLevel(INFO), toLogStr)
import           Issues.Lib.Web.ApiV1 (API_V1, apiV1Server)
import           Issues.Lib.Web.Site (SiteAPI, siteAPIServer)

type API = SiteAPI :<|> API_V1

api :: ServerT API AppHandler
api = siteAPIServer :<|> apiV1Server

_app :: JwtKey -> RunTime -> Logger -> Application
_app key rt logger = serveWithContextT p context nt api
  where
    p :: Proxy API
    p = Proxy

    context = authContext key

    nt :: forall a. AppHandler a -> Handler a
    nt = runAppHandler rt logger

type RequestId = UUID

-- add the request id to the logged message
withReqId :: Logger -> RequestId -> Logger
withReqId logger reqId level msg = logger level ("[" <> reqId' <> "]" <> msg)
  where reqId' = toLogStr (toASCIIBytes reqId)

timeDiff :: TimeSpec -> TimeSpec -> Double
timeDiff start end = milliSecs
  where
    diff = diffTimeSpec start end
    nanoSecs :: Double = fromIntegral $ toNanoSecs diff
    milliSecs = nanoSecs / 1000000.0

pathLogStr :: Request -> LogStr
pathLogStr req =
  toLogStr (requestMethod req)
  <> " "
  <> toLogStr (rawPathInfo req)
  <> toLogStr (rawQueryString req)

app :: JwtKey -> RunTime -> Logger -> Application
app key rt logger req res = do
  startTime <- getTime Monotonic
  reqId <- nextRandom
  let logger' = withReqId logger reqId
  logger' INFO (pathLogStr req)
  _app key rt logger' req $ \response -> do
    endTime <- getTime Monotonic
    let diff = toLogStr $ timeDiff startTime endTime
    logger' INFO ("request elapsed ms: " <> diff)
    res response

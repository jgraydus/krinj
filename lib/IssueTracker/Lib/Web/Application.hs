module IssueTracker.Lib.Web.Application (
    app
) where

import           Data.UUID (toASCIIBytes, UUID)
import           Data.UUID.V4 (nextRandom)
import           Network.Wai (rawQueryString, rawPathInfo, Request, requestMethod)
import           Servant
import           System.Clock (Clock(Monotonic), diffTimeSpec, getTime, TimeSpec, toNanoSecs)
import           System.Log.FastLogger (LogStr)

import           IssueTracker.Lib.Auth (authContext)
import           IssueTracker.Lib.Config (ApplicationConfig(..))
import           IssueTracker.Lib.ContentTypes ()
import           IssueTracker.Lib.Language.RunTime (AppHandler, runAppHandler, RunTime)
import           IssueTracker.Lib.Logger (Logger, LogLevel(INFO), toLogStr)
import           IssueTracker.Lib.Web.ApiV1 (API_V1, apiV1Server)
import           IssueTracker.Lib.Web.Site (SiteAPI, siteAPIServer)

type API = API_V1 :<|> SiteAPI

api :: ServerT API AppHandler
api =  apiV1Server :<|> siteAPIServer

_app :: ApplicationConfig -> RunTime -> Logger -> Application
_app config rt logger = serveWithContextT p context nt api
  where
    ApplicationConfig {..} = config

    p :: Proxy API
    p = Proxy

    context = authContext _applicationConfigJwtKey 

    nt :: forall a. AppHandler a -> Handler a
    nt = runAppHandler rt logger config

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

app :: ApplicationConfig -> RunTime -> Logger -> Application
app config rt logger req res = do
  startTime <- getTime Monotonic
  reqId <- nextRandom
  let logger' = withReqId logger reqId
  logger' INFO (pathLogStr req)
  _app config rt logger' req $ \response -> do
    endTime <- getTime Monotonic
    let diff = toLogStr $ timeDiff startTime endTime
    logger' INFO ("request elapsed ms: " <> diff)
    res response


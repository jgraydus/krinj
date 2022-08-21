module Issues.Lib.Web.Application (
    app
) where

import Servant

import           Issues.Lib.Auth (authContext)
import           Issues.Lib.Config (JwtKey)
import           Issues.Lib.ContentTypes ()
import           Issues.Lib.Language.RunTime (AppHandler, runAppHandler, RunTime)
import           Issues.Lib.Web.ApiV1 (API_V1, apiV1Server)
import           Issues.Lib.Web.Site (SiteAPI, siteAPIServer)

type API = SiteAPI :<|> API_V1

api :: ServerT API AppHandler
api = siteAPIServer :<|> apiV1Server

app :: JwtKey -> RunTime -> Application
app key rt = serveWithContextT p context nt api
  where
    p :: Proxy API
    p = Proxy

    context = authContext key

    nt :: forall a. AppHandler a -> Handler a
    nt = runAppHandler rt


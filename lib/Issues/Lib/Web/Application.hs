module Issues.Lib.Web.Application (
    app
) where

import Servant

import qualified Issues.Lib.Language as L
import           Issues.Lib.Auth
import           Issues.Lib.Config
import           Issues.Lib.ContentTypes ()
import           Issues.Lib.Web.ApiV1
import           Issues.Lib.Web.Site

type API = SiteAPI :<|> API_V1

api :: L.DB -> Server API
api db = siteAPIServer :<|> apiV1Server db

app :: JwtKey -> L.DB -> Application
app key db = serveWithContext (Proxy :: Proxy API) (authContext key) (api db)


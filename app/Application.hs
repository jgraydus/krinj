module Application (
    app
) where

import ApiV1
import Auth
import Config
import ContentTypes
import qualified Language as L
import Network.Wai (Application)
import Site
import Servant
import Servant.Server

type API = SiteAPI :<|> API_V1

api :: L.DB -> Server API
api db = siteAPIServer :<|> apiV1Server db

app :: JwtKey -> L.DB -> Application
app key db = serveWithContext (Proxy :: Proxy API) (authContext key) (api db)


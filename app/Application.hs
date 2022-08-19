module Application (
    app
) where

import ApiV1
import Auth
import Config
import ContentTypes
import Network.Wai (Application)
import Site
import Servant
import Servant.Server

type API = SiteAPI :<|> API_V1

api :: Server API
api = siteAPIServer :<|> apiV1Server

app :: JwtKey -> Application
app key = serveWithContext (Proxy :: Proxy API) (authContext key) api


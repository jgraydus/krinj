module Application (
    app
) where

import ApiV1
import ContentTypes
import Network.Wai (Application)
import Site
import Servant
import Servant.Server

type API = SiteAPI :<|> API_V1

api :: Server API
api = siteAPIServer :<|> apiV1Server

app :: Application
app = serve (Proxy :: Proxy API) api


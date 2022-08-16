module Application (
    app
) where

import ContentTypes
import Network.Wai (Application)
import Site
import Servant
import Servant.Server

type API = Index :<|> Bundle

api :: Server API
api = index_ :<|> bundle_

app :: Application
app = serve (Proxy :: Proxy API) api


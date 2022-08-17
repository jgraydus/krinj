module Site (
    SiteAPI, siteAPIServer
) where

import ContentTypes
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import JsBundle
import NeatInterpolation
import Servant
import Servant.Server

type Index = Get '[HTML] Text

index_ :: Handler Text
index_ = pure $ [text|
  <!doctype HTML>
  <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>foobar</title> 
    </head>
    <body>
      <div id="root"></div>
      <script src="bundle.js"></script>
    </body>
  </html>
|]


type Bundle = "bundle.js" :> Get '[JavaScript] Text

bundle_ :: Handler Text
bundle_ = liftIO jsBundle


type SiteAPI = Index :<|> Bundle

siteAPIServer :: Server SiteAPI
siteAPIServer = index_ :<|> bundle_


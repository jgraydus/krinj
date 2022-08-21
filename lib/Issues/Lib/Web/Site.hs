module Issues.Lib.Web.Site (
    SiteAPI, siteAPIServer
) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import NeatInterpolation
import Servant

import Issues.Lib.ContentTypes
import Issues.Lib.Language.RunTime
import Issues.Lib.Web.JsBundle

type Index = Get '[HTML] Text

index_ :: AppHandler Text
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

bundle_ :: AppHandler Text
bundle_ = liftIO jsBundle


type SiteAPI = Index :<|> Bundle

siteAPIServer :: ServerT SiteAPI AppHandler
siteAPIServer = index_ :<|> bundle_


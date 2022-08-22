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

-- | serve the index.html file. this route will accept any path because
--   the client does frontend routing (which manipulates the URL). consequently,
--   this should be the last configured route
type Index = CaptureAll "ignored" Text :> Get '[HTML] Text

index :: [Text] -> AppHandler Text
index _ = pure $ [text|
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

bundle :: AppHandler Text
bundle = liftIO jsBundle

type SiteAPI = Bundle :<|> Index

siteAPIServer :: ServerT SiteAPI AppHandler
siteAPIServer = bundle :<|> index


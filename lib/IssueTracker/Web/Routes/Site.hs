module IssueTracker.Web.Routes.Site (
    SiteAPI, siteAPIServer
) where

import Control.Monad.Reader (asks)
import Data.Text (pack, Text)
import IssueTracker.BuildUtils (commitHash)
import IssueTracker.Config (ApplicationConfig(..), HttpServerConfig(..),
                            PortNumber(..), Protocol(..), HostName(..))
import IssueTracker.Web.ContentTypes (CSS, HTML, JavaScript)
import IssueTracker.Web.Routes.Site.Bundles qualified as Bundles
import IssueTracker.Web.RouteHandler
import GHC.Records (getField)
import NeatInterpolation (text)
import Servant.API

bundlePath :: HttpServerConfig -> Text
bundlePath config = protocol <> "://" <> host <> port <> "/bundle"
  where
    Protocol protocol = config.protocol
    HostName host = config.host
    PortNumber port' = config.port
    port :: Text = if port' == 0 then "" else ":" <> (pack . show) port'

-- | serve the index.html file. this route will accept any path because
--   the client does frontend routing (which manipulates the URL). consequently,
--   this should be the last configured route
type Index = CaptureAll "ignored" Text :> Get '[HTML] Text

index :: RouteHandler Index
index _ = do
  ApplicationConfig {..} <- asks (getField @"applicationConfig")
  let path = bundlePath httpServerConfig
  pure $ [text|
    <!doctype HTML>
    <html lang="en">
      <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Issue Tracker</title>
        <link rel="stylesheet" href="${path}.css">
        <script>
          window.IssueTracker = { commitHash: "${commitHash}" }
        </script>
      </head>
      <body>
        <div id="root"></div>
        <script src="${path}.js"></script>
      </body>
    </html>
  |]

type JsBundle = "bundle.js" :> Get '[JavaScript] Text

jsBundle :: RouteHandler JsBundle
jsBundle = Bundles.jsBundle

type CssBundle = "bundle.css" :> Get '[CSS] Text

cssBundle :: RouteHandler CssBundle
cssBundle = Bundles.cssBundle

type SiteAPI = JsBundle :<|> CssBundle :<|> Index

siteAPIServer :: RouteHandler SiteAPI
siteAPIServer = jsBundle :<|> cssBundle :<|> index


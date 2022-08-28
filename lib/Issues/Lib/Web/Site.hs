module Issues.Lib.Web.Site (
    SiteAPI, siteAPIServer
) where

import           Control.Monad.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (pack, Text)
import           NeatInterpolation
import           Servant

import           Issues.Lib.BuildUtils (commitHash)
import           Issues.Lib.Config (ApplicationConfig(..), HttpConfig(..))
import           Issues.Lib.ContentTypes
import           Issues.Lib.Language.RunTime
import qualified Issues.Lib.Web.Bundles as Bundles

bundlePath :: HttpConfig -> Text
bundlePath HttpConfig {..} = pack $
  _httpConfigProtocol
  <> "://"
  <> _httpConfigHost
  <> (if length port == 0 then "" else ":" <> port)
  <> "/bundle"
  where
    port = if _httpConfigPort == 0 then "" else show _httpConfigPort

 
-- | serve the index.html file. this route will accept any path because
--   the client does frontend routing (which manipulates the URL). consequently,
--   this should be the last configured route
type Index = CaptureAll "ignored" Text :> Get '[HTML] Text

index :: [Text] -> AppHandler Text
index _ = do
  (_, _, ApplicationConfig {..}) <- ask
  let path = bundlePath _applicationConfigHttp
  pure $ [text|
    <!doctype HTML>
    <html lang="en">
      <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>foobar</title>
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

jsBundle :: AppHandler Text
jsBundle = liftIO Bundles.jsBundle

type CssBundle = "bundle.css" :> Get '[CSS] Text

cssBundle :: AppHandler Text
cssBundle = liftIO Bundles.cssBundle

type SiteAPI = JsBundle :<|> CssBundle :<|> Index

siteAPIServer :: ServerT SiteAPI AppHandler
siteAPIServer = jsBundle :<|> cssBundle :<|> index


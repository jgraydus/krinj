module Issues.Lib.Web.Site (
    SiteAPI, siteAPIServer
) where

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack, Text)
import NeatInterpolation
import Servant

import Issues.Lib.BuildUtils (commitHash)
import Issues.Lib.Config (ApplicationConfig(..), HttpConfig(..))
import Issues.Lib.ContentTypes
import Issues.Lib.Language.RunTime
import Issues.Lib.Web.JsBundle

bundlePath :: HttpConfig -> Text
bundlePath HttpConfig {..} = pack $
  _httpConfigProtocol
  <> "://"
  <> _httpConfigHost
  <> (if length port == 0 then "" else ":" <> port)
  <> "/bundle.js"
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
        <script>
          window.IssueTracker = { commitHash: "${commitHash}" }
        </script>
      </head>
      <body>
        <div id="root"></div>
        <script src="${path}"></script>
      </body>
    </html>
  |]

type Bundle = "bundle.js" :> Get '[JavaScript] Text

bundle :: AppHandler Text
bundle = liftIO jsBundle

type SiteAPI = Bundle :<|> Index

siteAPIServer :: ServerT SiteAPI AppHandler
siteAPIServer = bundle :<|> index


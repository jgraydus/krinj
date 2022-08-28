module Issues.Lib.ContentTypes (
    CSS, HTML, JavaScript
) where

import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Media ((//), (/:))
import Servant.API.ContentTypes (Accept(..),MimeRender(..))

-- this module defines types and typeclass instances to enable
-- specifying servant endpoints that return HTML and javascript

-----------------------------------------------------------------------
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "Utf-8")

instance MimeRender HTML Text where
  mimeRender _ = BSL.fromStrict . encodeUtf8

-----------------------------------------------------------------------
data JavaScript

instance Accept JavaScript where
  contentType _ = "application" // "javascript" /: ("charset", "Utf-8")

instance MimeRender JavaScript Text where
  mimeRender _ = BSL.fromStrict . encodeUtf8

-----------------------------------------------------------------------
data CSS

instance Accept CSS where
  contentType _ = "text" // "css" /: ("charset", "Utf-8")

instance MimeRender CSS Text where
  mimeRender _ = BSL.fromStrict . encodeUtf8


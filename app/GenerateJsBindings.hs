module GenerateJsBindings where

import ApiV1 
import Data.Proxy
import Data.Text (Text)
import Servant.JS
import Servant.JS.Axios

api :: Proxy API_V1
api = Proxy

options :: AxiosOptions
options = AxiosOptions
  { withCredentials = False
  , xsrfCookieName = Nothing
  , xsrfHeaderName = Nothing
  }

writeJSCode :: IO ()
writeJSCode = writeJSForAPI api (axios options) "client/src/generated/bindings.js"


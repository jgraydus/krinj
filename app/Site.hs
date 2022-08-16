module Site (
    Index, index_, Bundle, bundle_
) where

import ContentTypes
import Data.Text (Text)
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
bundle_ = error "NOT IMPLEMENTED YET"


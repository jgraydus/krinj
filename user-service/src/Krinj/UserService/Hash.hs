module Krinj.UserService.Hash where

import Crypto.Hash (Digest, hash, SHA3_512)
import Data.Text (pack, Text)
import Data.Text.Encoding (encodeUtf8)
import Krinj.Config (Password(..), Salt(..))

hashPassword :: Password -> Salt -> Text
hashPassword (Password password) (Salt salt) =
  let bs = encodeUtf8 (salt <> password)
      h  = hash bs :: Digest SHA3_512
  in pack (show h)


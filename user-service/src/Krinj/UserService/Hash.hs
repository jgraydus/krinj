module Krinj.UserService.Hash where

import Crypto.Hash (Digest, hash, SHA3_512)
import Data.Text (pack, Text)
import Data.Text.Encoding (encodeUtf8)
import Krinj.UserService.Types

hashPassword :: Monad m => Password -> m Text
hashPassword password = do
  let bs = encodeUtf8 password
      h  = hash bs :: Digest SHA3_512
      s  = pack (show h)
  pure s  


module Auth where

import Config
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson
import Data.ByteString (ByteString, fromStrict, toStrict)
import Data.Functor.Identity (runIdentity)
import qualified Data.List as L
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.UUID.V4 (nextRandom)
import Jose.Jwa (JwsAlg(HS256))
import Jose.Jws (hmacDecode, hmacEncode)
import Jose.Jwt (Jwt(..))
import Model
import Network.Wai (Request, requestHeaders)
import Servant (throwError)
import Servant.Server (Context(..), err401, errBody, ServerError)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

type JwtAuth = AuthProtect "JWT"
type instance AuthServerData JwtAuth = User

getAuthToken :: (MonadIO m, MonadError ServerError m) => Request -> m ByteString
getAuthToken req = do
  let headers = requestHeaders req
  case L.find ((== "x-bearer-token") . fst) headers of
    Just (_name, value) -> return value
    _ -> throwError $ err401 { errBody = "missing x-bearer-token header" }

authHandler :: JwtKey -> AuthHandler Request User
authHandler key = mkAuthHandler handler
  where
    handler req = do
      token <- getAuthToken req
      readAuthToken key token

authContext :: JwtKey -> Context (AuthHandler Request User ': '[])
authContext key = authHandler key :. EmptyContext


makeAuthToken :: JwtKey -> User -> ByteString
makeAuthToken key user = runIdentity $ do
  let k = (encodeUtf8 . pack) key
      bs = (toStrict . encode) user
  case hmacEncode HS256 k bs of
    Right (Jwt jwt) -> return jwt
    Left e -> error (show e)
 
readAuthToken :: (MonadIO m, MonadError ServerError m) => JwtKey -> ByteString -> m User
readAuthToken key jwt = do
  let k = (encodeUtf8 . pack) key
  case hmacDecode k jwt of
    Right (_, jwt) -> do
      case decode (fromStrict jwt) of
        Just user -> return user
        Nothing -> throwError $ err401 { errBody = "failed to parse token" }
    Left e -> throwError $ err401 { errBody = (fromStrict . encodeUtf8 . pack . show) e }


module Krinj.Web.Auth where

import Control.Monad ((<=<))
import Control.Exception (throw)
import Data.Aeson (decode', encode, FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (find)
import Data.Text.Encoding (encodeUtf8)
import Jose.Jwa (JwsAlg(HS256))
import Jose.Jws qualified as Jws
import Jose.Jwt (Jwt(..))
import Krinj.Config.Newtypes (JwtKey(..))
import Network.HTTP.Types (hCookie)
import Network.HTTP.Types.Header (Header)
import Network.Wai (Request, requestHeaders)
import Web.Cookie

isCookie :: Header -> Bool
isCookie = (== hCookie) . fst

getCookies :: Request -> Cookies
getCookies = parseCookies <=< (map snd . filter isCookie . requestHeaders)

isAuthCookie :: (ByteString, ByteString) -> Bool
isAuthCookie = (=="authorization") . fst

getAuthCookie :: Request -> Maybe ByteString
getAuthCookie = fmap snd . find isAuthCookie . getCookies

-- | parse the authorization token out of the request and convert it into the type a
getFromRequest :: FromJSON a => JwtKey -> Request -> Maybe a
getFromRequest (JwtKey key) req = do
  authCookie <- getAuthCookie req
  case Jws.hmacDecode (encodeUtf8 key) authCookie of
    Left _ -> Nothing
    Right (_, decodedTokenBS) -> decode' (LBS.fromStrict decodedTokenBS)

encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = LBS.toStrict . encode

-- | encode the given value of type a into an authorization cookie
makeAuthTokenCookie :: ToJSON a => JwtKey -> a -> SetCookie
makeAuthTokenCookie (JwtKey key) token = 
  case Jws.hmacEncode HS256 (encodeUtf8 key) (encodeStrict token) of
    Right (Jwt jwt) -> defaultSetCookie
                       { setCookieName = "authorization"
                       , setCookieValue = jwt
                       , setCookieHttpOnly = True
                       , setCookieSameSite = Just sameSiteStrict
                       -- TODO , setCookieSecure = True
                       }
    Left _ -> throw $ userError "failed to generate auth token for cookie"


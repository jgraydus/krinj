module Krinj.Web.Routes.Authentication (
    AuthenticationApi, authenticationApiHandler
) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON)
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Generics (Generic)
import GHC.Records (getField)
import Krinj.Config (ApplicationConfig(..), HttpServerConfig(..))
import Krinj.UserService (findUserByCredentials)
import Krinj.UserService.Types (EmailAddress, Password, User(..))
import Krinj.Web.Auth (makeAuthTokenCookie)
import Krinj.Web.RouteHandler
import Servant.API
import Servant.Server (err403)
import Web.Cookie (defaultSetCookie, sameSiteStrict, SetCookie(..))

-----------------------------------------------------------------------------------------
type AuthenticationApi = LogIn :<|> LogOut

authenticationApiHandler :: RouteHandler AuthenticationApi
authenticationApiHandler = logInHandler :<|> logOutHandler

-----------------------------------------------------------------------------------------
-- POST /login

data LogInBody =
  LogInBody
  { emailAddress :: EmailAddress
  , password :: Password
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

type LogIn  =
     "login"
  :> ReqBody '[JSON] LogInBody 
  :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] User)

logInHandler :: RouteHandler LogIn
logInHandler LogInBody { emailAddress, password } = do
  userMaybe <- findUserByCredentials emailAddress password
  case userMaybe of
    Nothing -> throwError err403
    Just user -> do
      ApplicationConfig { httpServerConfig } <- asks (getField @"applicationConfig")
      let cookie = makeAuthTokenCookie httpServerConfig.jwtKey user.userId
      pure $ addHeader cookie user

-----------------------------------------------------------------------------------------
-- POST /logout

type LogOut =
     "logout"
  :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] ())

expiration :: UTCTime
expiration = fromJust $ iso8601ParseM "1970-01-01T00:00:00.000Z"

logOutHandler :: RouteHandler LogOut
logOutHandler = pure $ addHeader cookie ()
  where
    cookie :: SetCookie
    cookie = defaultSetCookie
             { setCookieName = "authorization"
             , setCookieValue = ""
             , setCookieHttpOnly = True
             , setCookieSameSite = Just sameSiteStrict
             , setCookieExpires = Just expiration
             -- TODO , setCookieSecure = True
             }


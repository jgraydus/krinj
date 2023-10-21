module Krinj.Web.Routes.Auth (
    AuthApi, authHandler
) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Krinj.UserService.Types (EmailAddress, Password, User)
import Krinj.Web.RouteHandler
import Servant

-----------------------------------------------------------------------------------------
type AuthApi = LogIn :<|> LogOut

authHandler :: RouteHandler AuthApi
authHandler = logInHandler :<|> logOutHandler

-----------------------------------------------------------------------------------------
-- POST /login

data LogInBody =
  LogInBody
  { emailAddress :: EmailAddress
  , password :: Password
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

type LogIn = "login" :> ReqBody '[JSON] LogInBody :> Post '[JSON] User

logInHandler :: RouteHandler LogIn
logInHandler = error "not implemented"

-----------------------------------------------------------------------------------------
-- POST /logout

type LogOut = "logout" :> Post '[JSON] ()

logOutHandler :: RouteHandler LogOut
logOutHandler = error "not implemented"


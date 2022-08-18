module Auth where

import Model
import Network.Wai (Request)
import Servant.Server (Context(..))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

type JwtAuth = AuthProtect "JWT"
type instance AuthServerData JwtAuth = User

authHandler :: AuthHandler Request User
authHandler = mkAuthHandler handler
  where handler req = return User

authContext :: Context (AuthHandler Request User ': '[])
authContext = authHandler :. EmptyContext


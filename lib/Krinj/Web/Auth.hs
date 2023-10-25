module Krinj.Web.Auth (
    AuthRequired,
    getFromRequest,
    makeAuthTokenCookie
) where

import Krinj.Config
import Krinj.UserService.Types (UserId)
import Krinj.Web.Auth.Token
import Krinj.Web.RequestContext
import Servant
import Servant.Server.Internal.Delayed (addAuthCheck)
import Servant.Server.Internal.DelayedIO (delayedFailFatal, DelayedIO, withRequest)

authorize :: RequestContext -> DelayedIO UserId
authorize reqCxt = withRequest $ \req -> do
  let RequestContext { applicationConfig } = reqCxt
      ApplicationConfig { httpServerConfig } = applicationConfig
      HttpServerConfig { jwtKey } = httpServerConfig
      userIdMaybe :: Maybe UserId = getFromRequest jwtKey req
  case userIdMaybe of
    Nothing -> delayedFailFatal err401
    Just userId -> pure userId

data AuthRequired

instance
  (HasServer api cxt, HasContextEntry cxt RequestContext)
  => HasServer (AuthRequired :> api) cxt
  where

  type ServerT (AuthRequired :> api) m = UserId -> ServerT api m

  route _ ctx s =
    let reqCtx = getContextEntry ctx
        p :: Proxy api = Proxy
    in  route p ctx (addAuthCheck s $ authorize reqCtx)

  hoistServerWithContext _ p2 nt s =
    let p1 :: Proxy api = Proxy
    in  hoistServerWithContext p1 p2 nt . s


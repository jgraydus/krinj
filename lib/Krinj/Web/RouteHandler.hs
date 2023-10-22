{-# LANGUAGE UndecidableInstances #-}
module Krinj.Web.RouteHandler where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import GHC.Records (HasField)
import Krinj.Config (ApplicationConfig)
import Krinj.EntityService
import Krinj.UserService
import Krinj.Web.Routes.Site.Bundles (SiteBundles)
import Servant

-- | RouteHandler
-- This type synonym is used for all route handler functions. Note that
-- instead of specifying a specific m (monad) and r (reader input), it
-- only specifies constraints on these types. Also, MonadIO is deliberately
-- not included as a constraint, so route handlers cannot do arbitrary IO.
-- However, the concrete m that the server uses has a MonadIO instance.
-- This enables instances Of e.g. UserService to do IO. See the HasThreadDelay
-- class and instance below for a simple example. 
--type RouteHandler api = forall r m. Constraints r m => ServerT api m
type RouteHandler api = forall r m. Constraints r m => ServerT api m

type Constraints r m =
  ( Monad m
  , MonadError ServerError m
  , MonadReader r m
  , HasField "applicationConfig" r ApplicationConfig
  , EntityService m
  , SiteBundles m
  , UserService m
  )


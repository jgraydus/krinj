module Krinj.Web.RequestContext where

import Data.Pool (Pool)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import Krinj.Config
import Krinj.Logger (Logger)

type RequestId = UUID

data RequestContext = RequestContext
  { requestId :: RequestId
  , applicationConfig :: ApplicationConfig
  , logger :: Logger
  , databaseConnectionPool :: Pool Connection
  }


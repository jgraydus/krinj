{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Krinj.UserService.Instance where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (asks, MonadReader)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import GHC.Records (getField, HasField)
import Krinj.Config (ApplicationConfig(..))
import Krinj.UserService.Class
import Krinj.UserService.Types (EmailAddress, Password, User, UserId)
import Krinj.UserService.Command qualified as Command
import Krinj.UserService.Query qualified as Query

type Reqs r m = ( Monad m
                , MonadIO m
                , MonadReader r m
                , HasField "applicationConfig" r ApplicationConfig
                , HasField "databaseConnectionPool" r (Pool Connection)
                )

withConnection :: Reqs r m => (Connection -> IO a) -> m a
withConnection action = do
  pool <- asks (getField @"databaseConnectionPool")
  liftIO $ withResource pool action

instance Reqs r m => UserService m where

  createUser :: EmailAddress -> Password -> m (Maybe User)
  createUser emailAddress password = do
    ApplicationConfig { salt } <- asks (getField @"applicationConfig")
    withConnection $ \conn ->
      Command.createUser conn emailAddress password salt

  findUserById :: UserId -> m (Maybe User)
  findUserById userId = withConnection $ \conn ->
    Query.findUserById conn userId

  findUserByCredentials :: EmailAddress -> Password -> m (Maybe User)
  findUserByCredentials emailAddress password = do
    ApplicationConfig { salt } <- asks (getField @"applicationConfig")
    withConnection $ \conn ->
      Query.findUserByCredentials conn emailAddress password salt
  

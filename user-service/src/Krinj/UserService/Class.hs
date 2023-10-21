module Krinj.UserService.Class where

import Krinj.UserService.Types

class Monad m => UserService m where
  createUser            :: EmailAddress -> Password -> m (Maybe User)
  findUserById          :: UserId -> m (Maybe User)
  findUserByCredentials :: EmailAddress -> Password -> m (Maybe User)


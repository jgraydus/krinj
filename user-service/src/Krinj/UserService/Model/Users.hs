{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Krinj.UserService.Model.Users (
    UsersRow(..), usersTable
) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable')
import Opaleye (Field, optionalTableField, table, Table, tableField, SqlBool, SqlText, SqlUuid) 

data UsersRow a b c d =
  UsersRow
  { userId :: a
  , emailAddress :: b
  , isVerified :: c
  , isDeleted :: d
  }
$(makeAdaptorAndInstanceInferrable' ''UsersRow)

type UsersWrite = UsersRow (Maybe (Field SqlUuid))
                           (Field SqlText)
                           (Maybe (Field SqlBool))
                           (Maybe (Field SqlBool))

type UsersRead  = UsersRow (Field SqlUuid)
                           (Field SqlText)
                           (Field SqlBool)
                           (Field SqlBool)

usersTable :: Table UsersWrite UsersRead
usersTable =
  table "users" (pUsersRow
    UsersRow
    { userId        = optionalTableField "user_id"
    , emailAddress  = tableField "email_address"
    , isVerified    = optionalTableField "is_verified"
    , isDeleted     = optionalTableField "is_deleted"
    })


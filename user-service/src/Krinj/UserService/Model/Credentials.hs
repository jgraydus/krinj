{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Krinj.UserService.Model.Credentials (
    CredentialsRow(..), credentialsTable
) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable')
import Opaleye (Field, FieldNullable, optionalTableField, table, Table, tableField, SqlBool, SqlText, SqlUuid)

data CredentialsRow a b c =
  CredentialsRow
  { userId :: a
  , passwordHash :: b
  , isDeleted :: c
  }
$(makeAdaptorAndInstanceInferrable' ''CredentialsRow)

type CredentialsWrite = CredentialsRow (Maybe (Field SqlUuid))
                                       (FieldNullable SqlText)
                                       (Maybe (Field SqlBool))

type CredentialsRead  = CredentialsRow (Field SqlUuid)
                                       (FieldNullable SqlText)
                                       (Field SqlBool)

credentialsTable :: Table CredentialsWrite CredentialsRead
credentialsTable =
  table "credentials" (pCredentialsRow
    CredentialsRow
    { userId = optionalTableField "user_id"
    , passwordHash = tableField "password_hash"
    , isDeleted = optionalTableField "is_deleted"
    })


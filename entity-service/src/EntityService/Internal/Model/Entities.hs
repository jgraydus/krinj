{-# LANGUAGE DerivingVia, DeriveAnyClass, TemplateHaskell #-}
module EntityService.Internal.Model.Entities where

import Data.Aeson (FromJSON, ToJSON)
import Data.Profunctor.Product.TH
import Data.Time.Clock (UTCTime)
import EntityService.Internal.Model.Newtypes
import GHC.Generics (Generic)
import Opaleye

data EntitiesRowT a b c d e = EntitiesRowT
  {  entityId :: a
  ,  projectId :: b
  ,  entityTypeId :: c
  , createdAt :: d
  , modifiedAt :: e
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

$(makeAdaptorAndInstance' ''EntitiesRowT)

type EntitiesRowW =
  EntitiesRowT (Maybe (Field SqlUuid)) (Field SqlUuid) (Field SqlUuid)
               (Maybe (Field SqlTimestamptz)) (Maybe (FieldNullable SqlTimestamptz))
type EntitiesRowR =
  EntitiesRowT (Field SqlUuid) (Field SqlUuid) (Field SqlUuid)
               (Field SqlTimestamptz) (FieldNullable SqlTimestamptz)

entitiesTable :: Table EntitiesRowW EntitiesRowR
entitiesTable = table "entities" $
  pEntitiesRowT EntitiesRowT
  { entityId     = optionalTableField "entity_id"
  , projectId    = tableField "project_id"
  , entityTypeId = tableField "entity_type_id"
  , createdAt   = optionalTableField "created_at"
  , modifiedAt  = optionalTableField "modified_at"
  }

type Entity = EntitiesRowT EntityId ProjectId EntityTypeId UTCTime (Maybe UTCTime)


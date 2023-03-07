{-# LANGUAGE TemplateHaskell #-}
module EntityService.Internal.Model.Entities where

import Data.Profunctor.Product.TH
import EntityService.Internal.Model.Newtypes
import Opaleye

data EntitiesRowT a b c = EntitiesRowT
  {  entityId :: a
  ,  projectId :: b
  ,  entityTypeId :: c
  }

$(makeAdaptorAndInstance' ''EntitiesRowT)

type EntitiesRowW = EntitiesRowT (Maybe (Field SqlUuid)) (Field SqlUuid) (Field SqlUuid)
type EntitiesRowR = EntitiesRowT (Field SqlUuid) (Field SqlUuid) (Field SqlUuid)

entitiesTable :: Table EntitiesRowW EntitiesRowR
entitiesTable = table "entities" $
  pEntitiesRowT EntitiesRowT
  { entityId     = optionalTableField "entity_id"
  , projectId    = tableField "project_id"
  , entityTypeId = tableField "entity_type_id"
  }

type Entity = EntitiesRowT EntityId ProjectId EntityTypeId


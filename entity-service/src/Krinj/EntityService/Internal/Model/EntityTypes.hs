{-# LANGUAGE DerivingVia, DeriveAnyClass, TemplateHaskell #-}
module Krinj.EntityService.Internal.Model.EntityTypes where

import Data.Aeson (FromJSON, ToJSON)
import Data.Profunctor.Product.TH
import GHC.Generics (Generic)
import Krinj.EntityService.Internal.Model.Newtypes
import Opaleye

data EntityTypesRowT a b c d = EntityTypesRowT
  { entityTypeId :: a
  , projectId :: b
  , name :: c
  , descriptor :: d
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

$(makeAdaptorAndInstance' ''EntityTypesRowT)

type EntityTypesRowW = EntityTypesRowT (Maybe (Field SqlUuid)) (Field SqlUuid) (Field SqlText) (Field SqlJson) 
type EntityTypesRowR = EntityTypesRowT (Field SqlUuid) (Field SqlUuid) (Field SqlText) (Field SqlJson) 

entityTypesTable :: Table EntityTypesRowW EntityTypesRowR
entityTypesTable = table "entity_types" $
  pEntityTypesRowT EntityTypesRowT
  { entityTypeId = optionalTableField "entity_type_id"
  , projectId    = tableField "project_id"
  , name         = tableField "name"
  , descriptor   = tableField "descriptor"
  }

type EntityType = EntityTypesRowT EntityTypeId ProjectId EntityTypeName EntityTypeDescriptor


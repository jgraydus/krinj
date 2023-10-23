{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Krinj.EntityService.Internal.Model.Relationships where

import Data.Aeson (FromJSON, ToJSON)
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable')
import GHC.Generics (Generic)
import Krinj.EntityService.Internal.Model.Newtypes
import Opaleye

data RelationshipsRowT a b c d = RelationshipsRowT
  { relationshipId :: a
  , subjectId :: b
  , objectId :: c
  , relationshipType :: d
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

$(makeAdaptorAndInstanceInferrable' ''RelationshipsRowT)

type RelationshipsRowW = RelationshipsRowT (Maybe (Field SqlUuid)) (Field SqlUuid) (Field SqlUuid) (Field SqlText)
type RelationshipsRowR = RelationshipsRowT (Field SqlUuid) (Field SqlUuid) (Field SqlUuid) (Field SqlText)

relationshipsTable :: Table RelationshipsRowW RelationshipsRowR
relationshipsTable = table "relationships" $
  pRelationshipsRowT RelationshipsRowT
  { relationshipId     = optionalTableField "relationship_id"
  , subjectId          = tableField "subject_id"
  , objectId           = tableField "object_id"
  , relationshipType   = tableField "relationship_type"
  }

type Relationship = RelationshipsRowT RelationshipId EntityId EntityId RelationshipType


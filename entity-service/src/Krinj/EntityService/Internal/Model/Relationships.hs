{-# LANGUAGE TemplateHaskell #-}
module Krinj.EntityService.Internal.Model.Relationships where

import Data.Profunctor.Product.TH
import Krinj.EntityService.Internal.Model.Newtypes
import Opaleye

data RelationshipsRowT a b c d = RelationshipsRowT
  { relationshipId :: a
  , subjectId :: b
  , objectId :: c
  , relationship :: d
  }

$(makeAdaptorAndInstance' ''RelationshipsRowT)

type RelationshipsRowW = RelationshipsRowT (Maybe (Field SqlUuid)) (Field SqlUuid) (Field SqlUuid) (Field SqlText)
type RelationshipsRowR = RelationshipsRowT (Field SqlUuid) (Field SqlUuid) (Field SqlUuid) (Field SqlText)

relationshipsTable :: Table RelationshipsRowW RelationshipsRowR
relationshipsTable = table "relationships" $
  pRelationshipsRowT RelationshipsRowT
  { relationshipId = optionalTableField "relationship_id"
  , subjectId      = tableField "subject_id"
  , objectId       = tableField "object_id"
  , relationship   = tableField "relationship"
  }

type Relationship = RelationshipsRowT RelationshipId EntityId EntityId RelationshipType


module Krinj.EntityService.Internal.Command.Relationships (
    createRelationship, deleteRelationship
) where

import Database.PostgreSQL.Simple (Connection)
import GHC.Records (getField)
import Krinj.EntityService.Internal
import Krinj.EntityService.Internal.Model
import Opaleye

-- |
createRelationship :: Connection -> EntityId -> EntityId -> RelationshipType -> IO (Result Relationship)
createRelationship conn subjectId objectId relationshipType = do
  result <- runInsert conn insert
  pure $ case result of
    relationship : _ -> Right relationship
    _                -> Left InsertFailed
  where
    insert = Insert
      { iTable = relationshipsTable
      , iRows = [RelationshipsRowT Nothing (toFields subjectId) (toFields objectId) (toFields relationshipType)]
      , iReturning = rReturning id
      , iOnConflict = Nothing
      }

-- |
deleteRelationship :: Connection -> RelationshipId -> IO (Result ())
deleteRelationship conn relationshipId = do
  deleteCount <- runDelete conn delete
  pure $ if deleteCount == 1 then Right () else Left DeleteFailed
  where
    delete = Delete
      { dTable = relationshipsTable
      , dWhere = (.== toFields relationshipId) . (getField @"relationshipId")
      , dReturning = rCount
      }


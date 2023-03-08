module EntityService.Internal.Command.Relationships (
    createRelationship, deleteRelationship
) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import EntityService.Internal
import EntityService.Internal.Model
import GHC.Records (getField)
import Opaleye

-- |
createRelationship :: Connection -> EntityId -> EntityId -> Text -> IO (Result Relationship)
createRelationship conn subjectId objectId description = do
  result <- runInsert conn insert
  pure $ case result of
    relationship : _ -> Right relationship
    _                -> Left InsertFailed
  where
    insert = Insert
      { iTable = relationshipsTable
      , iRows = [RelationshipsRowT Nothing (toFields subjectId) (toFields objectId) (toFields description)]
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

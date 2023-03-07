module EntityService.Internal.Command.Entities (
    createEntity, updateEntity, deleteEntity
) where

import Database.PostgreSQL.Simple (Connection)
import EntityService.Internal
import EntityService.Internal.Model
import GHC.Records (getField)
import Opaleye

createEntity :: Connection -> ProjectId -> EntityTypeId -> IO (Result Entity)
createEntity conn projectId entityTypeId = do
  result <- runInsert conn insert
  pure $ case result of
    entity : _ -> Right entity
    _          -> Left InsertFailed
  where
    insert = Insert
      { iTable = entitiesTable
      , iRows = [EntitiesRowT Nothing (toFields projectId) (toFields entityTypeId)]
      , iReturning = rReturning id
      , iOnConflict = Nothing
      }

updateEntity :: Connection -> EntityId -> Maybe ProjectId -> Maybe EntityTypeId -> IO (Result Entity)
updateEntity conn entityId' projectIdMaybe entityTypeIdMaybe = do
  result <- runUpdate conn update
  pure $ case result of
    entity : _ -> Right entity
    _          -> Left UpdateFailed
  where
    updateWith row@EntitiesRowT {..} = EntitiesRowT
      { projectId = maybe row.projectId toFields projectIdMaybe
      , entityTypeId = maybe row.entityTypeId toFields entityTypeIdMaybe
      , ..
      } 
    update = Update
      { uTable = entitiesTable
      , uUpdateWith = updateEasy updateWith
      , uWhere = (.== toFields entityId') . (getField @"entityId")
      , uReturning = rReturning id
      }

deleteEntity :: Connection -> EntityId -> IO (Result ())
deleteEntity conn entityId = do
  deleteCount <- runDelete conn delete
  pure $ if deleteCount == 1 then Right () else Left DeleteFailed
  where
    delete = Delete
      { dTable = entitiesTable
      , dWhere = (.== toFields entityId) . (getField @"entityId")
      , dReturning = rCount
      }


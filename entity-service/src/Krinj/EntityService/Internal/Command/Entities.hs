module Krinj.EntityService.Internal.Command.Entities (
    createEntity, updateEntity, deleteEntity
) where

import Data.Maybe (fromMaybe, listToMaybe)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import GHC.Records (getField)
import Krinj.EntityService.Internal
import Krinj.EntityService.Internal.Model
import Opaleye ( (.==), (.&&), Delete(..), Insert(..), rCount, rReturning, runDelete, runInsert
               , runSelect, runUpdate, selectTable, toFields, Update(..), updateEasy, where_)
import Opaleye.Exists (exists)

createEntity :: Connection -> ProjectId -> EntityTypeId -> IO (Result Entity)
createEntity conn projectId entityTypeId = do
  result <- runInsert conn insert
  pure $ case result of
    entity : _ -> Right entity
    _          -> Left InsertFailed
  where
    insert = Insert
      { iTable = entitiesTable
      , iRows = [EntitiesRowT Nothing (toFields projectId) (toFields entityTypeId) Nothing Nothing]
      , iReturning = rReturning id
      , iOnConflict = Nothing
      }

updateEntity :: Connection -> EntityId -> Maybe ProjectId -> Maybe EntityTypeId -> IO (Result Entity)
updateEntity conn entityId' projectIdMaybe entityTypeIdMaybe = withTransaction conn $ do
  -- we must ensure that the entity type belongs to the project

  -- first find the current entity values
  entityMaybe <- fmap listToMaybe $ runSelect conn $ do
    row <- selectTable entitiesTable
    where_ $ row.entityId .== toFields entityId'
    pure (row.projectId, row.entityTypeId)

  case entityMaybe of
    Nothing -> pure $ Left NotFound
    Just (currentProjectId, currentEntityTypeId) -> do
      -- when either argument for update is missing, use the current value
      let projectIdForUpdate = fromMaybe currentProjectId projectIdMaybe
          entityTypeIdForUpdate = fromMaybe currentEntityTypeId entityTypeIdMaybe
      -- verify that the entity type exists and belongs to the project
      [ok] <- runSelect conn $ exists $ do
        row <- selectTable entityTypesTable
        where_ $ row.projectId .== toFields projectIdForUpdate
             .&& row.entityTypeId .== toFields entityTypeIdForUpdate
      if ok then do
        -- do the update
        result <- runUpdate conn
          Update
          { uTable = entitiesTable
          , uUpdateWith = updateEasy $ \row ->
              row { projectId = toFields projectIdForUpdate
                  , entityTypeId = toFields entityTypeIdForUpdate }
          , uWhere = (.== toFields entityId') . (getField @"entityId")
          , uReturning = rReturning id
          }
        pure $ case result of
          entity : _ -> Right entity
          _          -> Left UpdateFailed
      else
        pure $ Left Other

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


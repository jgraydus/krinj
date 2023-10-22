module Krinj.EntityService.Internal.Command.EntityTypes (
    createEntityType, updateEntityType, deleteEntityType
) where

import Database.PostgreSQL.Simple (Connection)
import GHC.Records (getField)
import Krinj.EntityService.Internal
import Krinj.EntityService.Internal.Model
import Opaleye

createEntityType :: Connection -> ProjectId -> EntityTypeName -> EntityTypeDescriptor -> IO (Result EntityType)
createEntityType conn projectId entityTypeName entityTypeDescriptor = do
  result <- runInsert conn insert
  pure $ case result of
    entityType : _ -> Right entityType
    _              -> Left InsertFailed 
  where
    insert = Insert
      { iTable = entityTypesTable
      , iRows = [EntityTypesRowT Nothing (toFields projectId) (toFields entityTypeName) (toFields entityTypeDescriptor)]
      , iReturning = rReturning id
      , iOnConflict = Nothing }

updateEntityType :: Connection -> EntityTypeId -> Maybe EntityTypeName -> Maybe EntityTypeDescriptor -> IO (Result EntityType)
updateEntityType conn entityTypeId' nameMaybe descriptorMaybe = do
  result <- runUpdate conn update
  pure $ case result of
    entityType : _ -> Right entityType
    _              -> Left UpdateFailed 
  where
    updateWith row@EntityTypesRowT {..} = EntityTypesRowT
      { name = maybe row.name toFields nameMaybe
      , descriptor = maybe row.descriptor toFields descriptorMaybe
      , ..
      }
    update = Update
      { uTable = entityTypesTable
      , uUpdateWith = updateEasy updateWith
      , uWhere = (.== toFields entityTypeId') . (getField @"entityTypeId")
      , uReturning = rReturning id
      }

deleteEntityType :: Connection -> EntityTypeId -> IO (Result ())
deleteEntityType conn entityTypeId = do
  deleteCount <- runDelete conn delete
  pure $ if deleteCount == 1 then Right () else Left DeleteFailed
  where
    delete = Delete
      { dTable = entityTypesTable
      , dWhere = (.== toFields entityTypeId) . (getField @"entityTypeId")
      , dReturning = rCount
      }


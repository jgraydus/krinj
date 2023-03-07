module EntityService.Internal.Command.Attributes (
    createAttributes, updateAttribute, deleteAttribute
) where

import Database.PostgreSQL.Simple (Connection)
import EntityService.Internal
import EntityService.Internal.Model
import GHC.Records (getField)
import Opaleye

createAttributes :: Connection -> EntityId -> [(AttributeName, AttributeValue)] -> IO (Result [Attribute])
createAttributes conn entityId args = Right <$> runInsert conn insert
  where
    toRow (name, value) = AttributesRowT Nothing (toFields entityId) (toFields name) (toFields value)
    insert = Insert
      { iTable = attributesTable
      , iRows = map toRow args
      , iReturning = rReturning id
      , iOnConflict = Nothing
      }

updateAttribute :: Connection -> AttributeId -> Maybe AttributeName -> Maybe AttributeValue -> IO (Result Attribute)
updateAttribute conn attributeId' attributeNameMaybe attributeValueMaybe = do
  result <- runUpdate conn update
  pure $ case result of
    attribute : _ -> Right attribute
    _             -> Left UpdateFailed
  where
    updateWith row@AttributesRowT {..} = AttributesRowT
      { name = maybe row.name toFields attributeNameMaybe
      , value = maybe row.value toFields attributeValueMaybe
      , ..
      }
    update = Update
      { uTable = attributesTable
      , uUpdateWith = updateEasy updateWith
      , uWhere = (.== toFields attributeId') . (getField @"attributeId")
      , uReturning = rReturning id
      }

deleteAttribute :: Connection -> AttributeId -> IO (Result ())
deleteAttribute conn attributeId = do
  deleteCount <- runDelete conn delete
  pure $ if deleteCount == 1 then Right () else Left DeleteFailed
  where
    delete = Delete
      { dTable = attributesTable
      , dWhere = (.== toFields attributeId) . (getField @"attributeId")
      , dReturning = rCount
      }


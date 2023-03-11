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
    toRow (name, value) = AttributesRowT (toFields entityId) (toFields name) (toFields value)
    insert = Insert
      { iTable = attributesTable
      , iRows = map toRow args
      , iReturning = rReturning id
      , iOnConflict = Nothing
      }

updateAttribute :: Connection -> EntityId -> AttributeName -> AttributeValue -> IO (Result Attribute)
updateAttribute conn entityId' attributeName' attributeValue' = do
  result <- runUpdate conn update
  pure $ case result of
    attribute : _ -> Right attribute
    _             -> Left UpdateFailed
  where
    updateWith AttributesRowT {..} = AttributesRowT { value = toFields attributeValue', .. }
    entityIdEq = (.== toFields entityId') . (getField @"entityId")
    attributeNameEq = (.== toFields attributeName') . (getField @"name")
    update = Update
      { uTable = attributesTable
      , uUpdateWith = updateEasy updateWith
      , uWhere = \row -> entityIdEq row .&& attributeNameEq row
      , uReturning = rReturning id
      }

deleteAttribute :: Connection -> EntityId -> AttributeName -> IO (Result ())
deleteAttribute conn entityId attributeName = do
  deleteCount <- runDelete conn delete
  pure $ if deleteCount == 1 then Right () else Left DeleteFailed
  where
    entityIdEq = (.== toFields entityId) . (getField @"entityId")
    attributeNameEq = (.== toFields attributeName) . (getField @"name")
    delete = Delete
      { dTable = attributesTable
      , dWhere = \row -> entityIdEq row .&& attributeNameEq row
      , dReturning = rCount
      }


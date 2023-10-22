module Krinj.EntityService.Internal.Command.Attributes (
    createAttributes, updateAttribute, deleteAttribute
) where

import Database.PostgreSQL.Simple (Connection)
import GHC.Records (getField)
import Krinj.EntityService.Internal
import Krinj.EntityService.Internal.Model
import Opaleye
import Opaleye.Exists

createAttributes :: Connection -> EntityId -> [(AttributeName, AttributeValue)] -> IO (Result [Attribute])
createAttributes conn entityId args = Right <$> runInsert conn insert
  where
    toRow (name, value) = AttributesRowT (toFields entityId) (toFields name) (toFields value) Nothing Nothing
    insert = Insert
      { iTable = attributesTable
      , iRows = map toRow args
      , iReturning = rReturning id
      , iOnConflict = Nothing
      }

updateAttribute :: Connection -> EntityId -> AttributeName -> AttributeValue -> IO (Result Attribute)
updateAttribute conn entityId' attributeName' attributeValue' = do
  [attributeExists] :: [Bool] <- runSelect conn $ exists $ do
    row@(AttributesRowT entityId attributeName _ _ _) <- selectTable attributesTable
    where_ $ (entityId .== toFields entityId') .&& (attributeName .== toFields attributeName')
    pure row
  result <- if attributeExists
            then runUpdate conn update
            else runInsert conn insert
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
    insert = Insert
      { iTable = attributesTable
      , iRows = [AttributesRowT (toFields entityId') (toFields attributeName')
                                (toFields attributeValue') Nothing Nothing]
      , iReturning = rReturning id
      , iOnConflict = Nothing
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


module EntityService.Internal.Query.Attributes (
    getAttribute, getAttributes
) where

import Database.PostgreSQL.Simple (Connection)
import EntityService.Internal
import EntityService.Internal.Model
import Opaleye

-- |
getAttribute :: Connection -> AttributeId -> IO (Result Attribute)
getAttribute conn attributeId = do
  result <- runSelect conn select
  pure $ case result of
    attribute : _ -> Right attribute
    _             -> Left NotFound
  where
    select = do
      row@(AttributesRowT attributeId1 _ _ _) <- selectTable attributesTable
      where_ $ attributeId1 .== toFields attributeId
      pure row

-- |
getAttributes :: Connection -> EntityId -> IO (Result [Attribute])
getAttributes conn entityId = Right <$> runSelect conn select
  where
    select = do
      row@(AttributesRowT _ entityId1 _ _) <- selectTable attributesTable
      where_ $ entityId1 .== toFields entityId
      pure row


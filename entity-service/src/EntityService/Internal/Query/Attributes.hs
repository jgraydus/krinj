module EntityService.Internal.Query.Attributes (
    getAttribute, getAttributes
) where

import Data.Map.Strict (Map)
import Database.PostgreSQL.Simple (Connection)
import EntityService.Internal
import EntityService.Internal.Model
import EntityService.Internal.Util
import GHC.Records (getField)
import Opaleye hiding (groupBy)

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
getAttributes :: Connection -> [EntityId] -> IO (Result (Map EntityId [Attribute]))
getAttributes conn entityIds = do
  attributes :: [Attribute] <- runSelect conn select
  pure . Right $ groupBy (getField @"entityId") attributes
  where
    select = do
      row@(AttributesRowT _ entityId1 _ _) <- selectTable attributesTable
      where_ $ in_ (fmap toFields entityIds) entityId1
      pure row


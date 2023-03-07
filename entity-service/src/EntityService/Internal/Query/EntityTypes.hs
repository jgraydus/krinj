module EntityService.Internal.Query.EntityTypes (
    getEntityType, getEntityTypes
) where

import Database.PostgreSQL.Simple (Connection)
import EntityService.Internal
import EntityService.Internal.Model
import Opaleye

-- |
getEntityType :: Connection -> EntityTypeId -> IO (Result EntityType)
getEntityType conn entityTypeId = do
  result :: [EntityType] <- runSelect conn select
  pure $ case result of
    entityType : _ -> Right entityType
    _              -> Left NotFound
  where
    select :: Select EntityTypesRowR
    select = do
      row@(EntityTypesRowT entityTypeId' _ _ _) <- selectTable entityTypesTable
      where_ $ entityTypeId' .== toFields entityTypeId
      pure row

-- |
getEntityTypes :: Connection -> ProjectId -> IO (Result [EntityType])
getEntityTypes conn projectId = Right <$> runSelect conn select
  where
    select :: Select EntityTypesRowR
    select = do
      row@(EntityTypesRowT _ projectId' _ _) <- selectTable entityTypesTable
      where_ $ projectId' .== toFields projectId
      pure row
  

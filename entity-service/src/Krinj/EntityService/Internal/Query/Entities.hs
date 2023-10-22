module Krinj.EntityService.Internal.Query.Entities (
    getEntities, getEntity
) where

import Database.PostgreSQL.Simple (Connection)
import Krinj.EntityService.Internal
import Krinj.EntityService.Internal.Model
import Opaleye

-- |
getEntity :: Connection -> EntityId -> IO (Result Entity)
getEntity conn entityId = do
  result <- runSelect conn select
  pure $ case result of
    entity : _ -> Right entity
    _          -> Left NotFound
  where
    select = do
      row@(EntitiesRowT entityId1 _ _ _ _) <- selectTable entitiesTable
      where_ $ (entityId1 .== toFields entityId)
      pure row

-- |
getEntities :: Connection -> ProjectId -> IO (Result [Entity])
getEntities conn projectId = Right <$> runSelect conn select
  where
    select = do
      row@(EntitiesRowT _ projectId1 _ _ _) <- selectTable entitiesTable
      where_ $ (projectId1 .== toFields projectId)
      pure row


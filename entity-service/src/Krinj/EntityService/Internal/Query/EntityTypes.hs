module Krinj.EntityService.Internal.Query.EntityTypes (
    getEntityType, getEntityTypes
) where

import Data.Map.Strict (Map)
import Database.PostgreSQL.Simple (Connection)
import GHC.Records (getField)
import Krinj.EntityService.Internal
import Krinj.EntityService.Internal.Model
import Krinj.EntityService.Internal.Util (groupBy)
import Opaleye hiding (groupBy)

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
getEntityTypes :: Connection -> [ProjectId] -> IO (Result (Map ProjectId [EntityType]))
getEntityTypes conn projectIds = do
  entityTypes :: [EntityType] <- runSelect conn select
  let entityTypesByProjectId = groupBy (getField @"projectId") entityTypes
  pure $ Right entityTypesByProjectId
  where
    select :: Select EntityTypesRowR
    select = do
      row@(EntityTypesRowT _ projectId' _ _) <- selectTable entityTypesTable
      where_ $ in_ (fmap toFields projectIds) projectId'
      pure row
 

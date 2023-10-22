module Krinj.EntityService.Internal.Query.Relationships (
    getRelationship, getRelationships
) where

import Database.PostgreSQL.Simple (Connection)
import Krinj.EntityService.Internal
import Krinj.EntityService.Internal.Model
import Opaleye

-- |
getRelationship :: Connection -> RelationshipId -> IO (Result Relationship)
getRelationship conn relationshipId = do
  result <- runSelect conn select
  pure $ case result of
    relationship : _ -> Right relationship
    _                -> Left NotFound
  where
    select = do
      row@(RelationshipsRowT relationshipId1 _ _ _) <- selectTable relationshipsTable
      where_ $ relationshipId1 .== toFields relationshipId
      pure row

-- |
getRelationships :: Connection -> EntityId -> IO (Result [Relationship])
getRelationships conn entityId = Right <$> runSelect conn (select1 `unionAll` select2)
  where
    select1 = do
      row@(RelationshipsRowT _ subjectId _ _) <- selectTable relationshipsTable
      where_ $ subjectId .== toFields entityId
      pure row
    select2 = do
      row@(RelationshipsRowT _ _ objectId _) <- selectTable relationshipsTable
      where_ $ objectId .== toFields entityId
      pure row


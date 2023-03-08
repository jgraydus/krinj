module EntityService.Class where

import EntityService.Internal
import EntityService.Internal.Model

class Monad m => EntityService m where
  createProject :: ProjectName -> ProjectDescription -> m (Result Project)
  updateProject :: ProjectId -> Maybe ProjectName -> Maybe ProjectDescription -> m (Result Project)
  deleteProject :: ProjectId -> m (Result ())
  getProject    :: ProjectId -> m (Result Project)
  getProjects   :: Page -> m (Result [Project])

  createEntityType :: ProjectId -> EntityTypeName -> EntityTypeDescriptor -> m (Result EntityType)
  updateEntityType :: EntityTypeId -> Maybe EntityTypeName -> Maybe EntityTypeDescriptor -> m (Result EntityType)
  deleteEntityType :: EntityTypeId -> m (Result ())
  getEntityType    :: EntityTypeId -> m (Result EntityType)
  getEntityTypes   :: ProjectId -> m (Result [EntityType])

  createEntity :: ProjectId -> EntityTypeId -> m (Result Entity)
  updateEntity :: EntityId -> Maybe ProjectId -> Maybe EntityTypeId -> m (Result Entity)
  deleteEntity :: EntityId -> m (Result ())
  getEntity    :: EntityId -> m (Result Entity)
  getEntities  :: ProjectId -> m (Result [Entity])

  createAttributes :: EntityId -> [(AttributeName, AttributeValue)] -> m (Result [Attribute])
  updateAttribute  :: AttributeId -> Maybe AttributeName -> Maybe AttributeValue -> m (Result Attribute)
  deleteAttribute  :: AttributeId -> m (Result ())
  getAttribute     :: AttributeId -> m (Result Attribute)
  getAttributes    :: EntityId -> m (Result [Attribute])
  
  createRelationship :: EntityId -> EntityId -> RelationshipType -> m (Result Relationship)
  deleteRelationship :: RelationshipId -> m (Result ())
  getRelationship :: RelationshipId -> m (Result Relationship)
  getRelationships :: EntityId -> m (Result [Relationship])


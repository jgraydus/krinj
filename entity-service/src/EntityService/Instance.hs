{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module EntityService.Instance where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (asks, MonadReader)
import Data.Map (Map)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import EntityService.Class
import EntityService.Internal
import EntityService.Internal.Command qualified as Command
import EntityService.Internal.Model
import EntityService.Internal.Query qualified as Query
import GHC.Records (getField, HasField)

type Reqs r m = ( Monad m
                , MonadIO m
                , MonadReader r m
                , HasField "databaseConnectionPool" r (Pool Connection)
                )

withConnection :: Reqs r m => (Connection -> IO a) -> m a
withConnection action = do
  pool <- asks (getField @"databaseConnectionPool")
  liftIO $ withResource pool action

instance Reqs r m => EntityService m where

  -- Projects

  createProject :: ProjectName -> ProjectDescription -> m (Result Project)
  createProject projectName projectDescription =
    withConnection $ \conn ->
      Command.createProject conn projectName projectDescription

  updateProject :: ProjectId -> Maybe ProjectName -> Maybe ProjectDescription -> m (Result Project)
  updateProject projectId projectNameMaybe projectDescriptionMaybe =
    withConnection $ \conn ->
      Command.updateProject conn projectId projectNameMaybe projectDescriptionMaybe

  deleteProject :: ProjectId -> m (Result ())
  deleteProject projectId =
    withConnection $ \conn ->
      Command.deleteProject conn projectId

  getProject :: ProjectId -> m (Result Project)
  getProject projectId =
    withConnection $ \conn ->
      Query.getProject conn projectId

  getProjects :: Page -> m (Result [Project])
  getProjects page =
    withConnection $ \conn ->
      Query.getProjects conn page

  -- EntityTypes

  createEntityType :: ProjectId -> EntityTypeName -> EntityTypeDescriptor -> m (Result EntityType)
  createEntityType projectId entityTypeName entityTypeDescriptor =
    withConnection $ \conn ->
      Command.createEntityType conn projectId entityTypeName entityTypeDescriptor

  updateEntityType :: EntityTypeId -> Maybe EntityTypeName -> Maybe EntityTypeDescriptor -> m (Result EntityType)
  updateEntityType entityTypeId entityTypeNameMaybe entityTypeDescriptorMaybe =
    withConnection $ \conn ->
      Command.updateEntityType conn entityTypeId entityTypeNameMaybe entityTypeDescriptorMaybe

  deleteEntityType :: EntityTypeId -> m (Result ())
  deleteEntityType entityTypeId =
    withConnection $ \conn ->
      Command.deleteEntityType conn entityTypeId

  getEntityType :: EntityTypeId -> m (Result EntityType)
  getEntityType entityTypeId =
    withConnection $ \conn ->
      Query.getEntityType conn entityTypeId

  getEntityTypes :: [ProjectId] -> m (Result (Map ProjectId [EntityType]))
  getEntityTypes projectIds =
    withConnection $ \conn ->
      Query.getEntityTypes conn projectIds

  -- Entities

  createEntity :: ProjectId -> EntityTypeId -> m (Result Entity)
  createEntity projectId entityTypeId =
    withConnection $ \conn ->
      Command.createEntity conn projectId entityTypeId

  updateEntity :: EntityId -> Maybe ProjectId -> Maybe EntityTypeId -> m (Result Entity)
  updateEntity entityId projectIdMaybe entityTypeIdMaybe =
    withConnection $ \conn ->
      Command.updateEntity conn entityId projectIdMaybe entityTypeIdMaybe

  deleteEntity :: EntityId -> m (Result ())
  deleteEntity entityId =
    withConnection $ \conn ->
      Command.deleteEntity conn entityId

  getEntity :: EntityId -> m (Result Entity)
  getEntity entityId =
    withConnection $ \conn ->
      Query.getEntity conn entityId

  getEntities :: ProjectId -> m (Result [Entity])
  getEntities projectId =
    withConnection $ \conn ->
      Query.getEntities conn projectId

-- | Attributes

  createAttributes :: EntityId -> [(AttributeName, AttributeValue)] -> m (Result [Attribute])
  createAttributes entityId args =
    withConnection $ \conn ->
      Command.createAttributes conn entityId args

  updateAttribute :: EntityId -> AttributeName -> AttributeValue -> m (Result Attribute)
  updateAttribute entityId attributeName attributeValue =
    withConnection $ \conn ->
      Command.updateAttribute conn entityId attributeName attributeValue

  deleteAttribute :: EntityId -> AttributeName -> m (Result ())
  deleteAttribute entityId attributeName =
    withConnection $ \conn ->
      Command.deleteAttribute conn entityId attributeName

  getAttribute :: EntityId -> AttributeName -> m (Result Attribute)
  getAttribute entityId attributeName =
    withConnection $ \conn ->
      Query.getAttribute conn entityId attributeName

  getAttributes :: [EntityId] -> m (Result (Map EntityId [Attribute]))
  getAttributes entityIds =
    withConnection $ \conn ->
      Query.getAttributes conn entityIds

-- | Relationships

  createRelationship :: EntityId -> EntityId -> RelationshipType -> m (Result Relationship)
  createRelationship subjectId objectId relationshipType =
    withConnection $ \conn ->
      Command.createRelationship conn subjectId objectId relationshipType

  deleteRelationship :: RelationshipId -> m (Result ())
  deleteRelationship relationshipId =
    withConnection $ \conn ->
      Command.deleteRelationship conn relationshipId

  getRelationship :: RelationshipId -> m (Result Relationship)
  getRelationship relationshipId =
    withConnection $ \conn ->
      Query.getRelationship conn relationshipId

  getRelationships :: EntityId -> m (Result [Relationship])
  getRelationships entityId = 
    withConnection $ \conn ->
      Query.getRelationships conn entityId


{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module EntityService.Instance where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (asks, MonadReader)
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

  getEntityTypes :: ProjectId -> m (Result [EntityType])
  getEntityTypes projectId =
    withConnection $ \conn ->
      Query.getEntityTypes conn projectId

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

  updateAttribute :: AttributeId -> Maybe AttributeName -> Maybe AttributeValue -> m (Result Attribute)
  updateAttribute attributeId attributeNameMaybe attributeValueMaybe =
    withConnection $ \conn ->
      Command.updateAttribute conn attributeId attributeNameMaybe attributeValueMaybe

  deleteAttribute :: AttributeId -> m (Result ())
  deleteAttribute attributeId =
    withConnection $ \conn ->
      Command.deleteAttribute conn attributeId

  getAttribute :: AttributeId -> m (Result Attribute)
  getAttribute attributeId =
    withConnection $ \conn ->
      Query.getAttribute conn attributeId

  getAttributes :: EntityId -> m (Result [Attribute])
  getAttributes entityId =
    withConnection $ \conn ->
      Query.getAttributes conn entityId


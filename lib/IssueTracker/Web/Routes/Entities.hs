module IssueTracker.Web.Routes.Entities (
    EntitiesApi, entitiesApiHandler
) where

import Control.Arrow ((&&&))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Foldable (toList)
import Data.Either (fromRight)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Time.Clock (UTCTime)
import EntityService
import GHC.Generics (Generic)
import GHC.Records (getField)
import IssueTracker.Web.RouteHandler
import Servant

type EntitiesApi =
  "entities" :> (
       GetEntities
  :<|> GetEntity
  :<|> CreateEntity
  :<|> UpdateEntity
  :<|> DeleteEntity
  )

entitiesApiHandler :: RouteHandler EntitiesApi
entitiesApiHandler =
       getEntitiesHandler
  :<|> getEntityHandler
  :<|> createEntityHandler
  :<|> updateEntityHandler
  :<|> deleteEntityHandler

-----------------------------------------------------------------------------------------
data DecoratedEntity = DecoratedEntity
  { entityId :: EntityId
  , projectId :: ProjectId
  , entityType :: Maybe EntityType
  , attributes :: Map AttributeName AttributeValue
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

-- an entity should be considered modified when an associated attribute is modifed or created.
-- so in order to calculate the modifiedAt timestamp, take the latest timestamp among all
-- createdAt and modifiedAt timestamps for the entity itself and all of its attributes
getModifiedAt :: Entity -> [Attribute] -> UTCTime
getModifiedAt entity attributes = maximum times
  where
    times = entityTimes <> attributeTimes
    entityTimes = catMaybes [Just entity.createdAt, entity.modifiedAt]
    attributeTimes = created <> modified
    created  = map      (getField @"createdAt") attributes
    modified = mapMaybe (getField @"modifiedAt") attributes

-- package the entity up with its entity type and attributes
decorateEntities :: (EntityService m, Traversable t) => t Entity -> m (t DecoratedEntity)
decorateEntities entities = do
  entityTypesByProjectId :: Map ProjectId [EntityType] <- fromRight Map.empty <$> getEntityTypes projectIds
  attributesByEntityId :: map EntityId [Attribute]     <- fromRight Map.empty <$> getAttributes entityIds

  let entityTypes = concat $ Map.elems entityTypesByProjectId
      entityTypesByEntityTypeId :: Map EntityTypeId EntityType = indexBy (getField @"entityTypeId") entityTypes

      decorate entity =
        let attrs = Map.findWithDefault [] entity.entityId attributesByEntityId
            -- add createdAt and modifiedAt as synthentic attributes
            timestamps = Map.fromList
              [ (AttributeName "createdAt", AttributeValue . toJSON $ entity.createdAt)
              , (AttributeName "modifiedAt", AttributeValue . toJSON $ getModifiedAt entity attrs)
              ]
            attrsMap = Map.fromList $ fmap ((getField @"name") &&& (getField @"value")) attrs
        in DecoratedEntity
             { entityId = entity.entityId
             , projectId = entity.projectId
             , entityType = Map.lookup entity.entityTypeId entityTypesByEntityTypeId
             , attributes = Map.union attrsMap timestamps
             }

  pure $ fmap decorate entities

  where
    projectIds :: [ProjectId] = fmap (getField @"projectId") (toList entities)
    entityIds :: [EntityId] = fmap (getField @"entityId") (toList entities)

decorateEntity :: EntityService m => Entity -> m DecoratedEntity
decorateEntity entity = NonEmpty.head <$> decorateEntities (entity :| [])

-----------------------------------------------------------------------------------------
type GetEntities = QueryParam "projectId" ProjectId :> Get '[JSON] [DecoratedEntity]

getEntitiesHandler :: RouteHandler GetEntities
getEntitiesHandler Nothing = error "TODO proper error. projectId is required"
getEntitiesHandler (Just projectId) = do
  result <- getEntities projectId
  case result of
    Left _ -> error "TODO proper error response"
    Right entities -> decorateEntities entities

-----------------------------------------------------------------------------------------
type GetEntity = Capture "entityId" EntityId :> Get '[JSON] DecoratedEntity

getEntityHandler :: RouteHandler GetEntity
getEntityHandler entityId = do
  result <- getEntity entityId
  case result of
    Left _ -> error "TODO proper error response"
    Right entity -> decorateEntity entity

-----------------------------------------------------------------------------------------
type CreateEntity = ReqBody '[JSON] CreateEntityReqBody :> Post '[JSON] DecoratedEntity

data CreateEntityReqBody = CreateEntityReqBody
  { projectId :: ProjectId
  , entityTypeId :: EntityTypeId
  , attributes :: [(AttributeName, AttributeValue)]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

createEntityHandler :: RouteHandler CreateEntity
createEntityHandler CreateEntityReqBody {..} = do
  result <- runExceptT $ do
    entity <- ExceptT $ createEntity projectId entityTypeId
    _ <- ExceptT $ createAttributes (getField @"entityId" entity) attributes
    pure entity
  case result of
    Left _ -> error "TODO proper error response"
    Right entity -> decorateEntity entity

-----------------------------------------------------------------------------------------
type UpdateEntity =
     Capture "entityId" EntityId
  :> ReqBody '[JSON] UpdateEntityReqBody
  :> Patch '[JSON] Entity

data UpdateEntityReqBody = UpdateEntityReqBody
  { projectId :: Maybe ProjectId
  , entityTypeId :: Maybe EntityTypeId
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

updateEntityHandler :: RouteHandler UpdateEntity
updateEntityHandler entityId UpdateEntityReqBody {..} = do
  result <- updateEntity entityId projectId entityTypeId
  case result of
    Left _ -> error "TODO proper error response"
    Right entity -> pure entity

-----------------------------------------------------------------------------------------
type DeleteEntity = Capture "entityId" EntityId :> Delete '[JSON] ()

deleteEntityHandler :: RouteHandler DeleteEntity
deleteEntityHandler entityId = do
  result <- deleteEntity entityId
  case result of
    Left _ -> error "TODO proper error response"
    Right _ -> pure ()


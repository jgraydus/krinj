module IssueTracker.Web.Routes.Entities (
    EntitiesApi, entitiesApiHandler
) where

import Data.Aeson (FromJSON, ToJSON)
import EntityService
import GHC.Generics (Generic)
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
type GetEntities = QueryParam "projectId" ProjectId :> Get '[JSON] [Entity]

getEntitiesHandler :: RouteHandler GetEntities
getEntitiesHandler Nothing = error "TODO proper error. projectId is required"
getEntitiesHandler (Just projectId) = do
  result <- getEntities projectId
  case result of
    Left _ -> error "TODO proper error response"
    Right entities -> pure entities

-----------------------------------------------------------------------------------------
type GetEntity = Capture "entityId" EntityId :> Get '[JSON] Entity

getEntityHandler :: RouteHandler GetEntity
getEntityHandler entityId = do
  result <- getEntity entityId
  case result of
    Left _ -> error "TODO proper error response"
    Right entity -> pure entity

-----------------------------------------------------------------------------------------
type CreateEntity = ReqBody '[JSON] CreateEntityReqBody :> Post '[JSON] Entity

data CreateEntityReqBody = CreateEntityReqBody
  { projectId :: ProjectId
  , entityTypeId :: EntityTypeId
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

createEntityHandler :: RouteHandler CreateEntity
createEntityHandler CreateEntityReqBody {..} = do
  result <- createEntity projectId entityTypeId
  case result of
    Left _ -> error "TODO proper error response"
    Right entity -> pure entity

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


module IssueTracker.Web.Routes.EntityTypes (
    EntityTypesApi, entityTypesApiHandler
) where

import Data.Aeson (FromJSON, ToJSON)
import EntityService
import GHC.Generics (Generic)
import IssueTracker.Web.RouteHandler
import Servant

type EntityTypesApi =
  "entity-types" :> (
       GetEntityTypes
  :<|> GetEntityType
  :<|> CreateEntityType
  :<|> UpdateEntityType
  :<|> DeleteEntityType
  )

entityTypesApiHandler :: RouteHandler EntityTypesApi
entityTypesApiHandler =
       getEntityTypesHandler
  :<|> getEntityTypeHandler
  :<|> createEntityTypeHandler
  :<|> updateEntityTypeHandler
  :<|> deleteEntityTypeHandler

-----------------------------------------------------------------------------------------
type GetEntityTypes = QueryParam "projectId" ProjectId :> Get '[JSON] [EntityType]

getEntityTypesHandler :: RouteHandler GetEntityTypes
getEntityTypesHandler Nothing = error "projectId is required"   -- TODO proper error response
getEntityTypesHandler (Just projectId) = do
  result <- getEntityTypes projectId
  case result of
    Left _ -> undefined  -- TODO map errors into ServantError
    Right entityTypes -> pure entityTypes

-----------------------------------------------------------------------------------------
type GetEntityType = Capture "entityTypeId" EntityTypeId :> Get '[JSON] EntityType

getEntityTypeHandler :: RouteHandler GetEntityType
getEntityTypeHandler entityTypeId = do
  result <- getEntityType entityTypeId
  case result of
    Left _ -> undefined  -- TODO map errors into ServantError
    Right entityType -> pure entityType

-----------------------------------------------------------------------------------------
type CreateEntityType = ReqBody '[JSON] CreateEntityTypeReqBody :> Post '[JSON] EntityType

data CreateEntityTypeReqBody = CreateEntityTypeReqBody
  { projectId :: ProjectId
  , name :: EntityTypeName
  , descriptor :: EntityTypeDescriptor
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

createEntityTypeHandler :: RouteHandler CreateEntityType
createEntityTypeHandler CreateEntityTypeReqBody {..} = do
  result <- createEntityType projectId name descriptor
  case result of
    Left _ -> undefined  -- TODO map errors into ServantError
    Right entityType -> pure entityType
  
-----------------------------------------------------------------------------------------
type UpdateEntityType =
     Capture "entityTypeId" EntityTypeId
  :> ReqBody '[JSON] UpdateEntityTypeReqBody
  :> Patch '[JSON] EntityType

data UpdateEntityTypeReqBody = UpdateEntityTypeReqBody
  { name :: Maybe EntityTypeName
  , descriptor :: Maybe EntityTypeDescriptor
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

updateEntityTypeHandler :: RouteHandler UpdateEntityType
updateEntityTypeHandler entityTypeId UpdateEntityTypeReqBody {..} = do
  result <- updateEntityType entityTypeId name descriptor
  case result of
    Left _ -> undefined  -- TODO map errors into ServantError
    Right entityType -> pure entityType

-----------------------------------------------------------------------------------------
type DeleteEntityType = Capture "entityTypeId" EntityTypeId :> Delete '[JSON] ()

deleteEntityTypeHandler :: RouteHandler DeleteEntityType
deleteEntityTypeHandler entityTypeId = do
  result <- deleteEntityType entityTypeId
  case result of
    Left _ -> undefined  -- TODO map errors into ServantError
    Right _ -> pure ()


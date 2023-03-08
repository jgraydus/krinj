module IssueTracker.Web.Routes.Attributes (
    AttributesApi, attributesApiHandler
) where

import Data.Aeson (FromJSON, ToJSON)
import EntityService
import GHC.Generics (Generic)
import IssueTracker.Web.RouteHandler
import Servant

type AttributesApi =
  "attributes" :> (
       GetAttributes
  :<|> GetAttribute
  :<|> CreateAttributes
  :<|> UpdateAttribute
  :<|> DeleteAttribute
  )

attributesApiHandler :: RouteHandler AttributesApi
attributesApiHandler =
       getAttributesHandler
  :<|> getAttributeHandler
  :<|> createAttributesHandler
  :<|> updateAttributesHandler
  :<|> deleteAttributesHandler

-----------------------------------------------------------------------------------------
type GetAttributes = QueryParam "entityId" EntityId :> Get '[JSON] [Attribute]

getAttributesHandler :: RouteHandler GetAttributes
getAttributesHandler Nothing = error "TODO proper error. entityId required"
getAttributesHandler (Just entityId) = do
  result <- getAttributes entityId
  case result of
    Left _ -> error "TODO proper error"
    Right attributes -> pure attributes

-----------------------------------------------------------------------------------------
type GetAttribute = Capture "attributeId" AttributeId :> Get '[JSON] Attribute

getAttributeHandler :: RouteHandler GetAttribute
getAttributeHandler attributeId = do
  result <- getAttribute attributeId
  case result of
    Left _ -> error "TODO proper error"
    Right attribute -> pure attribute

-----------------------------------------------------------------------------------------
type CreateAttributes = ReqBody '[JSON] CreateAttributesReqBody :> Post '[JSON] [Attribute]

data CreateAttributesReqBody = CreateAttributesReqBody
  { entityId :: EntityId
  , attributes: :: [(AttributeName, AttributeValue)]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

createAttributesHandler :: RouteHandler CreateAttributes
createAttributesHandler CreateAttributesReqBody {..} = do
  result <- createAttribute entityId attributes
  case result of
    Left _ -> error "TODO proper error"
    Right attribute -> pure attribute
  
-----------------------------------------------------------------------------------------
type UpdateAttribute =
     Capture "attributeId" AttributeId
  :> ReqBody '[JSON] UpdateAttributeReqBody
  :> Patch '[JSON] Attribute

data UpdateAttributeReqBody
  { name :: Maybe AttributeName
  , value :: Maybe AttributeValue
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

updateAttributeHandler :: RouteHandler UpdateAttribute
updateAttributeHandler UpdateAttributeReqBody {..} = do
  result <- updateAttribute attributeId entityId attributes
  case result of
    Left _ -> error "TODO proper error"
    Right attribute -> pure attribute
  
-----------------------------------------------------------------------------------------
type DeleteAttribute = Capture "attributeId" AttributeId :> Delete '[JSON] ()

deleteAttributeHandler :: RouteHandler DeleteAttribute
deleteAttributeHandler attributeId = do
  result <- deleteAttribute attributeId
  case result of
    Left _ -> error "TODO proper error"
    Right _ -> pure ()


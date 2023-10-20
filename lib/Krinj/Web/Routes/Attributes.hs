module Krinj.Web.Routes.Attributes (
    AttributesApi, attributesApiHandler
) where

import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Map.Strict qualified as Map
import EntityService
import Krinj.Web.RouteHandler
import Servant

type AttributesApi =
  "entities" :> Capture "entityId" EntityId :> "attributes" :> (
       GetAttributes
  :<|> CreateAttributes
  :<|> UpdateAttributes
  :<|> DeleteAttribute
  )

attributesApiHandler :: RouteHandler AttributesApi
attributesApiHandler entityId =
       getAttributesHandler entityId
  :<|> createAttributesHandler entityId
  :<|> updateAttributeHandler entityId
  :<|> deleteAttributeHandler entityId

-----------------------------------------------------------------------------------------
type GetAttributes =
     QueryParam "name" AttributeName
  :> Get '[JSON] [Attribute]

getAttributesHandler :: EntityId -> RouteHandler GetAttributes
getAttributesHandler entityId Nothing = do
  result <- getAttributes [entityId]
  case result of
    Left _ -> error "TODO proper error"
    Right attributes -> pure $ Map.findWithDefault [] entityId attributes
getAttributesHandler entityId (Just attributeName) = do
  result <- getAttribute entityId attributeName
  case result of
    Left _ -> error "TODO proper error"
    Right attribute -> pure [attribute]

-----------------------------------------------------------------------------------------
type CreateAttributes = ReqBody '[JSON] [(AttributeName, AttributeValue)] :> Post '[JSON] [Attribute]

createAttributesHandler :: EntityId -> RouteHandler CreateAttributes
createAttributesHandler entityId attributes = do
  result <- createAttributes entityId attributes
  case result of
    Left _ -> error "TODO proper error"
    Right attribute -> pure attribute
  
-----------------------------------------------------------------------------------------
type UpdateAttributes = ReqBody '[JSON] [(AttributeName, AttributeValue)] :> Patch '[JSON] [Attribute]

updateAttributeHandler :: EntityId -> RouteHandler UpdateAttributes
updateAttributeHandler entityId attributes = do
  result <- runExceptT $ mapM (ExceptT . uncurry (updateAttribute entityId)) attributes
  case result of
    Left _ -> error "TODO proper error"
    Right attributes' -> pure attributes'
  
-----------------------------------------------------------------------------------------
type DeleteAttribute =
     QueryParam "name" AttributeName
  :> Delete '[JSON] ()

deleteAttributeHandler :: EntityId -> RouteHandler DeleteAttribute
deleteAttributeHandler entityId (Just attributeName) = do
  result <- deleteAttribute entityId attributeName
  case result of
    Left _ -> error "TODO proper error"
    Right _ -> pure ()
deleteAttributeHandler _ _ = error "attributeName is a required query param"


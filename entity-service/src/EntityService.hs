module EntityService (
    EntityService(..)
  , Attribute, AttributeId, AttributeName, AttributeValue
  , Entity, EntityId
  , EntityType, EntityTypeId, EntityTypeName, EntityTypeDescriptor
  , Project, ProjectId, ProjectName, ProjectDescription
  , Error(..), Result,

  module EntityService.Instance
) where

import EntityService.Class
import EntityService.Instance
import EntityService.Internal
import EntityService.Internal.Model


module EntityService (
    EntityService(..)
  , Project, ProjectId, ProjectName, ProjectDescription
  , Entity, EntityId
  , EntityType, EntityTypeId, EntityTypeName, EntityTypeDescriptor
  , Error(..), Result,

  module EntityService.Instance
) where

import EntityService.Class
import EntityService.Instance
import EntityService.Internal
import EntityService.Internal.Model


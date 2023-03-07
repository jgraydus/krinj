module EntityService (
    EntityService(..)
  , Project, ProjectId, ProjectName, ProjectDescription
--  , Entity, EntityId
--  , EntityType, EntityTypeName, EntityTypeDescriptor
  , Error(..), Result
) where

import EntityService.Class
import EntityService.Internal
import EntityService.Internal.Model


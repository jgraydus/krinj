{-# LANGUAGE DerivingVia, DeriveAnyClass #-}
module EntityService.Internal where

import Data.Aeson (FromJSON, ToJSON)
import EntityService.Internal.Model
import GHC.Generics (Generic)

{-
data Project = Project
  { projectId :: ProjectId
  , name :: ProjectName
  , description :: ProjectDescription
  } 
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
-}
{-
data EntityType = EntityType
  { entityTypeId :: EntityTypeId
  , name :: EntityTypeName
  , descriptor :: EntityTypeDescriptor
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Attribute = (AttributeName, AttributeValue)

data Entity = Entity
  { entityId :: EntityId
  , entityType :: EntityTypeName
  , attributes :: [Attribute]
  } 
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
-}
data Page = Page
  { pageNumber :: Int
  , resultsPerPage :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Error = NotFound | InsertFailed | UpdateFailed | DeleteFailed | Other
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Result a = Either Error a



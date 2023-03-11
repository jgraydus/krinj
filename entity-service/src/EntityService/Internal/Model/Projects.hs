{-# LANGUAGE DerivingVia, DeriveAnyClass, TemplateHaskell #-}
module EntityService.Internal.Model.Projects where

import Data.Aeson (FromJSON, ToJSON)
import Data.Profunctor.Product.TH
import Data.Time.Clock (UTCTime)
import EntityService.Internal.Model.Newtypes
import GHC.Generics (Generic)
import Opaleye

data ProjectsRowT a b c d e = ProjectsRowT
  { projectId :: a
  , name :: b
  , description :: c
  , createdAt :: d
  , modifiedAt :: e
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

$(makeAdaptorAndInstance' ''ProjectsRowT)

type ProjectsRowW =
  ProjectsRowT (Maybe (Field SqlUuid)) (Field SqlText) (Field SqlText)
               (Maybe (Field SqlTimestamptz)) (Maybe (FieldNullable SqlTimestamptz))
type ProjectsRowR =
  ProjectsRowT (Field SqlUuid) (Field SqlText) (Field SqlText)
               (Field SqlTimestamptz) (FieldNullable SqlTimestamptz)

projectsTable :: Table ProjectsRowW ProjectsRowR
projectsTable = table "projects" $
  pProjectsRowT ProjectsRowT
  { projectId   = optionalTableField "project_id"
  , name        = tableField "name"
  , description = tableField "description"
  , createdAt   = optionalTableField "created_at"
  , modifiedAt  = optionalTableField "modified_at"
  }

type Project = ProjectsRowT ProjectId ProjectName ProjectDescription UTCTime (Maybe UTCTime)


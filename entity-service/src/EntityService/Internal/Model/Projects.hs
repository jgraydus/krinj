{-# LANGUAGE DerivingVia, DeriveAnyClass, TemplateHaskell #-}
module EntityService.Internal.Model.Projects where

import Data.Aeson (FromJSON, ToJSON)
import Data.Profunctor.Product.TH
import EntityService.Internal.Model.Newtypes
import GHC.Generics (Generic)
import Opaleye

data ProjectsRowT a b c = ProjectsRowT
  { projectId :: a
  , name :: b
  , description :: c
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

$(makeAdaptorAndInstance' ''ProjectsRowT)

type ProjectsRowW = ProjectsRowT (Maybe (Field SqlUuid)) (Field SqlText) (Field SqlText)
type ProjectsRowR = ProjectsRowT (Field SqlUuid) (Field SqlText) (Field SqlText)

projectsTable :: Table ProjectsRowW ProjectsRowR
projectsTable = table "projects" $
  pProjectsRowT ProjectsRowT
  { projectId   = optionalTableField "project_id"
  , name        = tableField "name"
  , description = tableField "description"
  }

type Project = ProjectsRowT ProjectId ProjectName ProjectDescription


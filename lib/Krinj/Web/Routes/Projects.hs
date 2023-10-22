module Krinj.Web.Routes.Projects where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Either (fromRight)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import GHC.Records (getField)
import Krinj.EntityService
import Krinj.Web.RouteHandler
import Servant

type ProjectsApi =
  "projects" :> (
       GetProjects
  :<|> GetProject
  :<|> CreateProject
  :<|> UpdateProject
  :<|> DeleteProject
  )

projectsApiHandler :: RouteHandler ProjectsApi
projectsApiHandler =
       getProjectsHandler
  :<|> getProjectHandler
  :<|> createProjectHandler
  :<|> updateProjectHandler
  :<|> deleteProjectHandler

-----------------------------------------------------------------------------------------
data DecoratedProject = DecoratedProject
  { projectId :: ProjectId
  , name :: ProjectName
  , description :: ProjectDescription
  , entityTypes :: [EntityType]
  , createdAt :: UTCTime
  , modifiedAt :: Maybe UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

decorateProjects :: (Traversable t, Monad m, EntityService m) => t Project -> m (t DecoratedProject)
decorateProjects projects = do
  let projectIds :: [ProjectId] = fmap (getField @"projectId") (toList projects)
  entityTypesByProjectId <- fromRight Map.empty <$> getEntityTypes projectIds
  pure $ fmap (decorate entityTypesByProjectId) projects
  where
    decorate entityTypesByProjectId project =
      DecoratedProject
      { projectId = project.projectId
      , name = project.name
      , description = project.description
      , entityTypes = Map.findWithDefault [] project.projectId entityTypesByProjectId
      , createdAt = project.createdAt
      , modifiedAt = project.modifiedAt
      }

decorateProject :: (Monad m, EntityService m) => Project -> m DecoratedProject
decorateProject project = NonEmpty.head <$> decorateProjects (project :| [])

-----------------------------------------------------------------------------------------
type GetProjects = Get '[JSON] [DecoratedProject]

getProjectsHandler :: RouteHandler GetProjects
getProjectsHandler = do
  result <- fmap decorateProjects <$> getProjects undefined -- TODO paging 
  case result of
    Left _ -> undefined  -- TODO map errors into ServantError
    Right projects -> projects

-----------------------------------------------------------------------------------------
type GetProject = Capture "projectId" ProjectId :> Get '[JSON] DecoratedProject

getProjectHandler :: RouteHandler GetProject
getProjectHandler projectId = do
  result <- fmap decorateProject <$> getProject projectId
  case result of
    Left _ -> undefined -- TODO map erros into ServantError
    Right project -> project

-----------------------------------------------------------------------------------------
type CreateProject = ReqBody '[JSON] CreateProjectReqBody :> Post '[JSON] DecoratedProject

data CreateProjectReqBody = CreateProjectReqBody
  { name :: ProjectName
  , description :: ProjectDescription
  , entityTypes :: [(EntityTypeName, EntityTypeDescriptor)]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON) 

createProjectHandler :: RouteHandler CreateProject
createProjectHandler CreateProjectReqBody {..} = do
  result <- runExceptT $ do
    project <- ExceptT $ createProject name description
    forM_ entityTypes $ ExceptT . uncurry (createEntityType project.projectId)
    pure project
  case result of
    Left _ -> undefined  -- TODO map errors into ServantError
    Right project -> decorateProject project

-----------------------------------------------------------------------------------------
type UpdateProject =
     Capture "projectId" ProjectId
  :> ReqBody '[JSON] UpdateProjectReqBody
  :> Patch '[JSON] DecoratedProject

data UpdateProjectReqBody = UpdateProjectReqBody
  { name :: Maybe ProjectName
  , description :: Maybe ProjectDescription
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON) 

updateProjectHandler :: RouteHandler UpdateProject
updateProjectHandler projectId UpdateProjectReqBody {..} = do
  result <- updateProject projectId name description
  case result of
    Left _ -> undefined  -- TODO map errors into ServantError
    Right project -> decorateProject project

-----------------------------------------------------------------------------------------
type DeleteProject = Capture "projectId" ProjectId :> Delete '[JSON] ()

deleteProjectHandler :: RouteHandler DeleteProject
deleteProjectHandler projectId = do
  result <- deleteProject projectId
  case result of
    Left _ -> undefined -- TODO map erros into ServantError
    Right _ -> pure ()


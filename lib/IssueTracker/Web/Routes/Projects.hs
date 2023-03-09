module IssueTracker.Web.Routes.Projects where

import Data.Aeson (FromJSON, ToJSON)
import Data.Either (fromRight)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import EntityService
import GHC.Generics (Generic)
import GHC.Records (getField)
import IssueTracker.Web.RouteHandler
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
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON) 

createProjectHandler :: RouteHandler CreateProject
createProjectHandler CreateProjectReqBody {..} = do
  result <- createProject name description
  case result of
    Left _ -> undefined  -- TODO map errors into ServantError
    Right project -> decorateProject project

-----------------------------------------------------------------------------------------
type UpdateProject =
     Capture "projectId" ProjectId
  :> ReqBody '[JSON] UpdateProjectReqBody
  :> Patch '[JSON] Project

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
    Right project -> pure project

-----------------------------------------------------------------------------------------
type DeleteProject = Capture "projectId" ProjectId :> Delete '[JSON] ()

deleteProjectHandler :: RouteHandler DeleteProject
deleteProjectHandler projectId = do
  result <- deleteProject projectId
  case result of
    Left _ -> undefined -- TODO map erros into ServantError
    Right _ -> pure ()


module IssueTracker.Web.Routes.Projects where

import Data.Aeson (FromJSON, ToJSON)
import EntityService
import GHC.Generics (Generic)
import IssueTracker.Web.RouteHandler
import Servant

type ProjectsAPI =
       GetProjects
  :<|> GetProject
  :<|> CreateProject
  :<|> UpdateProject
  :<|> DeleteProject

projectsAPIHandler :: RouteHandler ProjectsAPI
projectsAPIHandler =
       getProjectsHandler
  :<|> getProjectHandler
  :<|> createProjectHandler
  :<|> updateProjectHandler
  :<|> deleteProjectHandler

-----------------------------------------------------------------------------------------
type GetProjects = "projects" :> Get '[JSON] [Project]

getProjectsHandler :: RouteHandler GetProjects
getProjectsHandler = do
  result <- getProjects undefined -- TODO paging
  case result of
    Left _ -> undefined  -- TODO map errors into ServantError
    Right projects -> pure projects

-----------------------------------------------------------------------------------------
type GetProject = "projects" :> Capture "projectId" ProjectId :> Get '[JSON] Project

getProjectHandler :: RouteHandler GetProject
getProjectHandler projectId = do
  result <- getProject projectId
  case result of
    Left _ -> undefined -- TODO map erros into ServantError
    Right project -> pure project

-----------------------------------------------------------------------------------------
type CreateProject = "projects" :> ReqBody '[JSON] CreateProjectReqBody :> Post '[JSON] Project

data CreateProjectReqBody = CreateProjectReqBody
  { projectName :: ProjectName
  , projectDescription :: ProjectDescription
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON) 

createProjectHandler :: RouteHandler CreateProject
createProjectHandler CreateProjectReqBody {..} = do
  result <- createProject projectName projectDescription
  case result of
    Left _ -> undefined  -- TODO map errors into ServantError
    Right project -> pure project

-----------------------------------------------------------------------------------------
type UpdateProject =
  "projects"
  :> Capture "projectId" ProjectId
  :> ReqBody '[JSON] UpdateProjectReqBody
  :> Patch '[JSON] Project

data UpdateProjectReqBody = UpdateProjectReqBody
  { projectName :: Maybe ProjectName
  , projectDescription :: Maybe ProjectDescription
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON) 

updateProjectHandler :: RouteHandler UpdateProject
updateProjectHandler projectId UpdateProjectReqBody {..} = do
  result <- updateProject projectId projectName projectDescription
  case result of
    Left _ -> undefined  -- TODO map errors into ServantError
    Right project -> pure project

-----------------------------------------------------------------------------------------
type DeleteProject = "projects" :> Capture "projectId" ProjectId :> Delete '[JSON] ()

deleteProjectHandler :: RouteHandler DeleteProject
deleteProjectHandler projectId = do
  result <- deleteProject projectId
  case result of
    Left _ -> undefined -- TODO map erros into ServantError
    Right _ -> pure ()


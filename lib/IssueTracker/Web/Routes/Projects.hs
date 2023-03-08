module IssueTracker.Web.Routes.Projects where

import Data.Aeson (FromJSON, ToJSON)
import EntityService
import GHC.Generics (Generic)
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
type GetProjects = Get '[JSON] [Project]

getProjectsHandler :: RouteHandler GetProjects
getProjectsHandler = do
  result <- getProjects undefined -- TODO paging
  case result of
    Left _ -> undefined  -- TODO map errors into ServantError
    Right projects -> pure projects

-----------------------------------------------------------------------------------------
type GetProject = Capture "projectId" ProjectId :> Get '[JSON] Project

getProjectHandler :: RouteHandler GetProject
getProjectHandler projectId = do
  result <- getProject projectId
  case result of
    Left _ -> undefined -- TODO map erros into ServantError
    Right project -> pure project

-----------------------------------------------------------------------------------------
type CreateProject = ReqBody '[JSON] CreateProjectReqBody :> Post '[JSON] Project

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
    Right project -> pure project

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


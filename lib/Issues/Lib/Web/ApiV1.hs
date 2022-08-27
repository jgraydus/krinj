module Issues.Lib.Web.ApiV1 where

import           Control.Monad.Reader (ask)
import           Servant

import qualified Issues.Lib.Language as L
import           Issues.Lib.Auth
import           Issues.Lib.Model
import           Issues.Lib.Language.RunTime

------------------------------------------------------------------------------------
------------------------------------------
-- Create project

type CreateProject = "create" :> Post '[JSON] Project

createProject :: User -> AppHandler Project
createProject user = do
  (RunTime {..}, logger, _) <- ask
  runApp logger user L.createProject

------------------------------------------
-- Delete Project

type DeleteProject = "delete" :> Capture "projectId" ProjectId :> Delete '[JSON] ()

deleteProject :: User -> ProjectId -> AppHandler ()
deleteProject user projectId = do
  (RunTime {..}, logger, _) <- ask
  runApp logger user (L.deleteProject projectId)

------------------------------------------
-- Get Project

type GetProject = Capture "projectId" ProjectId :> Get '[JSON] Project

getProject :: User -> ProjectId -> AppHandler Project
getProject user projectId = do
  (RunTime {..}, logger, _) <- ask
  runApp logger user (L.getProject projectId)

------------------------------------------
-- Get Projects

type GetProjects = Get '[JSON] [Project]

getProjects :: User -> AppHandler [Project]
getProjects user = do
  (RunTime {..}, logger, _) <- ask
  runApp logger user L.getProjects

------------------------------------------
-- Update Project

type UpdateProject = Capture "projectId" ProjectId :> ReqBody '[JSON] [ProjectUpdate] :> Patch '[JSON] Project

updateProject :: User -> ProjectId -> [ProjectUpdate] -> AppHandler Project
updateProject user projectId updates = do
  (RunTime {..}, logger, _) <- ask
  runApp logger user (L.updateProject projectId updates)

------------------------------------------------------------------------------------
------------------------------------------
-- Create issue

type CreateIssue = "create" :> ReqBody '[JSON] ProjectId :> Post '[JSON] Issue

createIssue :: User -> ProjectId -> AppHandler Issue
createIssue user projectId = do
  (RunTime {..}, logger, _) <- ask
  runApp logger user (L.createIssue projectId)

------------------------------------------
-- Delete issue

type DeleteIssue = "delete" :> Capture "issueId" IssueId :> Delete '[JSON] ()

deleteIssue :: User -> IssueId -> AppHandler ()
deleteIssue user issueId = do
  (RunTime {..}, logger, _) <- ask
  runApp logger user (L.deleteIssue issueId)

------------------------------------------
-- Get issue

type GetIssue = Capture "issueId" IssueId :> Get '[JSON] Issue

getIssue :: User -> IssueId -> AppHandler Issue
getIssue user issueId = do
  (RunTime {..}, logger, _) <- ask
  runApp logger user (L.getIssue issueId)

------------------------------------------
-- Get issues

type GetIssues = QueryParam "projectId" ProjectId :> QueryParam "parentId" IssueId :> Get '[JSON] [Issue]

getIssues :: User -> Maybe ProjectId -> Maybe IssueId -> AppHandler [Issue]
getIssues user _projectId _parentId = do
  (RunTime {..}, logger, _) <- ask
  runApp logger user L.getIssues

------------------------------------------
-- Update issue

type UpdateIssue = Capture "issueId" IssueId :> ReqBody '[JSON] [IssueUpdate] :> Patch '[JSON] Issue

updateIssue :: User -> IssueId -> [IssueUpdate] -> AppHandler Issue
updateIssue user issueId issueUpdates = do
  (RunTime {..}, logger, _) <- ask
  runApp logger user (L.updateIssue issueId issueUpdates)

------------------------------------------------------------------------------------
------------------------------------------
-- Create comment

type CreateComment = "create" :> ReqBody '[JSON] IssueId :> Post '[JSON] Comment

createComment :: User -> IssueId -> AppHandler Comment
createComment user issueId = do
  (RunTime {..}, logger, _) <- ask
  runApp logger user (L.createComment issueId)

------------------------------------------
-- Delete comment

type DeleteComment = "delete" :> Capture "commentId" CommentId :> Delete '[JSON] ()

deleteComment :: User -> CommentId -> AppHandler ()
deleteComment user commentId = do
  (RunTime {..}, logger, _) <- ask
  runApp logger user (L.deleteComment commentId)

------------------------------------------
-- Get comments

type GetComments = QueryParam "issueId" IssueId :> Get '[JSON] [Comment]

getComments :: User -> Maybe IssueId -> AppHandler [Comment]
getComments user issueId = do
  (RunTime {..}, logger, _) <- ask
  runApp logger user (L.getComments issueId)

------------------------------------------
-- Update comment

type UpdateComment = Capture "commentId" CommentId :> ReqBody '[JSON] [CommentUpdate] :> Patch '[JSON] Comment

updateComment :: User -> CommentId -> [CommentUpdate] -> AppHandler Comment
updateComment user commentId updates = do
  (RunTime {..}, logger, _) <- ask
  runApp logger user (L.updateComment commentId updates)

------------------------------------------------------------------------------------
------------------------------------------
-- API definition and composite handler

type ProjectsV1API = "projects" :> (CreateProject :<|> DeleteProject :<|> GetProject :<|> GetProjects :<|> UpdateProject)
type IssuesV1API = "issues" :> (CreateIssue :<|> DeleteIssue :<|> GetIssue :<|> GetIssues :<|> UpdateIssue)
type CommentsV1API = "comments" :> (CreateComment :<|> DeleteComment :<|> GetComments :<|> UpdateComment)

type API_V1 = JwtAuth :> "api" :> "v1" :> (ProjectsV1API :<|> IssuesV1API :<|> CommentsV1API)

apiV1Server :: ServerT API_V1 AppHandler
apiV1Server user = projectsV1Handler :<|> issuesV1Handler :<|> commentsV1Handler
  where
    projectsV1Handler =
      createProject user
      :<|> deleteProject user
      :<|> getProject user
      :<|> getProjects user
      :<|> updateProject user
    issuesV1Handler =
      createIssue user
      :<|> deleteIssue user
      :<|> getIssue user
      :<|> getIssues user
      :<|> updateIssue user
    commentsV1Handler =
      createComment user
      :<|> deleteComment user
      :<|> getComments user
      :<|> updateComment user



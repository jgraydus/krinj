module ApiV1 where

import           Auth
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import qualified Data.Aeson as Aeson
import           Data.Bson (genObjectId)
import           Data.Text (pack, Text)
import qualified Language as L
import           Model
import           Network.Wai (Request)
import           ObjectId
import           Servant
import           Servant.API.Experimental.Auth
import           Servant.Server
import           Servant.Server.Experimental.Auth

------------------------------------------------------------------------------------
------------------------------------------
-- Create project

type CreateProject = "create" :> Post '[JSON] Project

createProject :: L.DB -> User -> Handler Project
createProject db user = L.runApp db user L.createProject

------------------------------------------
-- Delete Project

type DeleteProject = "delete" :> Capture "projectId" ProjectId :> Delete '[JSON] ()

deleteProject :: L.DB -> User -> ProjectId -> Handler ()
deleteProject db user projectId = L.runApp db user (L.deleteProject projectId)

------------------------------------------
-- Get Project

type GetProject = Capture "projectId" ProjectId :> Get '[JSON] Project

getProject :: L.DB -> User -> ProjectId -> Handler Project
getProject db user projectId = L.runApp db user (L.getProject projectId)

------------------------------------------
-- Update Project

type UpdateProject = Capture "projectId" ProjectId :> ReqBody '[JSON] [ProjectUpdate] :> Patch '[JSON] Project

updateProject :: L.DB -> User -> ProjectId -> [ProjectUpdate] -> Handler Project
updateProject db user projectId updates = L.runApp db user (L.updateProject projectId updates)

------------------------------------------------------------------------------------
------------------------------------------
-- Create issue

type CreateIssue = "create" :> ReqBody '[JSON] ProjectId :> Post '[JSON] Issue

createIssue :: L.DB -> User -> ProjectId -> Handler Issue
createIssue db user projectId = L.runApp db user (L.createIssue projectId)

------------------------------------------
-- Delete issue

type DeleteIssue = "delete" :> Capture "issueId" IssueId :> Delete '[JSON] ()

deleteIssue :: L.DB -> User -> IssueId -> Handler ()
deleteIssue db user issueId = L.runApp db user (L.deleteIssue issueId)

------------------------------------------
-- Get issue

type GetIssue = Capture "issueId" IssueId :> Get '[JSON] Issue

getIssue :: L.DB -> User -> IssueId -> Handler Issue
getIssue db user issueId = L.runApp db user (L.getIssue issueId)

------------------------------------------
-- Update issue

type UpdateIssue = Capture "issueId" IssueId :> ReqBody '[JSON] [IssueUpdate] :> Patch '[JSON] Issue

updateIssue :: L.DB -> User -> IssueId -> [IssueUpdate] -> Handler Issue
updateIssue db user issueId issueUpdates = L.runApp db user (L.updateIssue issueId issueUpdates)

------------------------------------------------------------------------------------
------------------------------------------
-- Create comment

type CreateComment = "create" :> ReqBody '[JSON] IssueId :> Post '[JSON] Comment

createComment :: L.DB -> User -> IssueId -> Handler Comment
createComment db user issueId = L.runApp db user (L.createComment issueId)

------------------------------------------
-- Delete comment

type DeleteComment = "delete" :> Capture "commentId" CommentId :> Delete '[JSON] ()

deleteComment :: L.DB -> User -> CommentId -> Handler ()
deleteComment db user commentId = L.runApp db user (L.deleteComment commentId)

------------------------------------------
-- Get comments

type GetComments = QueryParam "issueId" IssueId :> Get '[JSON] [Comment]

getComments :: L.DB -> User -> Maybe IssueId -> Handler [Comment]
getComments db user issueId = L.runApp db user (L.getComments issueId)

------------------------------------------
-- Update comment

type UpdateComment = Capture "commentId" CommentId :> ReqBody '[JSON] [CommentUpdate] :> Patch '[JSON] Comment

updateComment :: L.DB -> User -> CommentId -> [CommentUpdate] -> Handler Comment
updateComment db user commentId updates = L.runApp db user (L.updateComment commentId updates)

------------------------------------------------------------------------------------
------------------------------------------
-- API definition and composite handler

type ProjectsV1API = "v1" :> "projects" :> (CreateProject :<|> DeleteProject :<|> GetProject :<|> UpdateProject)
type IssuesV1API = "v1" :> "issues" :> (CreateIssue :<|> DeleteIssue :<|> GetIssue :<|> UpdateIssue)
type CommentsV1API = "v1" :> "comments" :> (CreateComment :<|> DeleteComment :<|> GetComments :<|> UpdateComment)

type API_V1 = JwtAuth :> (ProjectsV1API :<|> IssuesV1API :<|> CommentsV1API)

apiV1Server :: L.DB -> Server API_V1
apiV1Server db user = projectsV1Handler :<|> issuesV1Handler :<|> commentsV1Handler
  where
    projectsV1Handler =
      createProject db user
      :<|> deleteProject db user
      :<|> getProject db user
      :<|> updateProject db user
    issuesV1Handler =
      createIssue db user
      :<|> deleteIssue db user
      :<|> getIssue db user
      :<|> updateIssue db user
    commentsV1Handler =
      createComment db user
      :<|> deleteComment db user
      :<|> getComments db user
      :<|> updateComment db user


module Issues.Lib.Language where

import           Control.Monad.Free (liftF)
import           Control.Exception (SomeException)

import           Issues.Lib.Language.AppL
import           Issues.Lib.Model

-- projects
createProject :: App Project
createProject = liftF $ CreateProject id

deleteProject :: ProjectId -> App ()
deleteProject projectId = liftF $ DeleteProject projectId id

getProject :: ProjectId -> App Project
getProject projectId = liftF $ GetProject projectId id

getProjects :: App [Project]
getProjects = liftF $ GetProjects id

updateProject :: ProjectId -> [ProjectUpdate] -> App Project
updateProject projectId updates = liftF $ UpdateProject projectId updates id

-- issues
createIssue :: ProjectId -> App Issue
createIssue projectId = liftF $ CreateIssue projectId id

deleteIssue :: IssueId -> App ()
deleteIssue issueId = liftF $ DeleteIssue issueId id

getIssue :: IssueId -> App Issue
getIssue issueId = liftF $ GetIssue issueId id

updateIssue :: IssueId -> [IssueUpdate] -> App Issue
updateIssue issueId updates = liftF $ UpdateIssue issueId updates id

-- comments
createComment :: IssueId -> App Comment
createComment issueId = liftF $ CreateComment issueId id

deleteComment :: CommentId -> App ()
deleteComment commentId = liftF $ DeleteComment commentId id

getComments :: Maybe IssueId -> App [Comment]
getComments issueId = liftF $ GetComments issueId id

updateComment :: CommentId -> [CommentUpdate] -> App Comment
updateComment commentId updates = liftF $ UpdateComment commentId updates id

-- misc
log :: String -> App ()
log msg = liftF $ Log msg id

err :: SomeException -> App a
err e = liftF $ Err e


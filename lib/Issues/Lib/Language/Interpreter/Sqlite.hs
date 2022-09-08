module Issues.Lib.Language.Interpreter.Sqlite (
  withRunTime
) where

import           Prelude hiding (log)

import           Control.Monad (forM_)
import           Control.Monad.Except (MonadError)
import           Control.Monad.Free (foldFree)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Bson (genObjectId)
import           Data.Time.Clock (getCurrentTime)
import           Database.SQLite.Simple (Connection, execute, execute_, executeNamed, NamedParam(..), Only(..),
                                         query, query_, withConnection)
import           Servant (throwError)
import           Servant.Server (err400, err404, err500, errBody, ServerError)
import           System.Log.FastLogger (toLogStr)

import           Issues.Lib.Language.AppL
import           Issues.Lib.Model
import           Issues.Lib.Language.RunTime
import           Issues.Lib.Logger (Logger, LogLevel(..))

interpret :: (MonadIO m, MonadError ServerError m) => Connection -> Logger -> User -> AppL a -> m a
interpret conn log user = \case
  -- projects
  CreateProject next -> do
    projectId <- liftIO genObjectId
    now <- liftIO getCurrentTime
    let project =
          Project
          { projectId
          , title = ""
          , description = ""
          , owner = _userId user 
          , createdBy = _userId user
          , createdAt = now
          , updatedAt = now
          }
    liftIO $ log DEBUG (toLogStr $ "CREATE project: " <> show projectId)
    liftIO $ execute conn
       "INSERT INTO projects (projectId, title, description, owner, createdBy, createdAt, updatedAt) \
       \VALUES (?, ?, ?, ?, ?, ?, ?)"
       project
    result :: [Project] <- liftIO $ query conn
      "SELECT projectId, title, description, owner, createdBy, createdAt, updatedAt \
      \FROM projects WHERE projectId = ?"
      (Only projectId)
    case result of
      [p] -> return $ next p
      _ -> throwError err500 { errBody = "problem creating project" }

  DeleteProject projectId next -> do
    liftIO $ log DEBUG (toLogStr $ "DELETE project: " <> show projectId)
    liftIO $ execute conn "DELETE FROM projects WHERE projectId = ?" (Only projectId)
    return $ next ()

  GetProject projectId next -> do
    liftIO $ log DEBUG (toLogStr $ "GET project: " <> show projectId)
    result :: [Project] <- liftIO $ query conn
      "SELECT projectId, title, description, owner, createdBy, createdAt, updatedAt \
      \FROM projects WHERE projectId = ?"
      (Only projectId)
    case result of
      [p] -> return $ next p 
      _   -> throwError $ err404 { errBody = "project not found" }

  GetProjects next -> do
    liftIO $ log DEBUG "GET projects"
    projects :: [Project] <- liftIO $ query_ conn
      "SELECT projectId, title, description, owner, createdBy, createdAt, updatedAt FROM projects"
    return $ next projects

  UpdateProject projectId updates next -> do
    liftIO $ log DEBUG (toLogStr $ "UDPATE project: " <> show projectId)
    now <- liftIO getCurrentTime
    liftIO $ forM_ updates $ \case
      ProjectTitle title -> executeNamed conn
        "UPDATE projects \
        \SET title = :title, updatedAt = :updatedAt \
        \WHERE projectId = :projectId" 
        [":projectId" := projectId, ":title" := title, ":updatedAt" := now]
      ProjectDescription description -> executeNamed conn
        "UPDATE projects \
        \SET description = :description, updatedAt = :updatedAt \
        \WHERE projectId = :projectId"
        [":projectId" := projectId, ":description" := description, ":updatedAt" := now]
      ProjectOwner owner -> executeNamed conn
        "UPDATE projects \
        \SET owner = :owner, updatedAt = :updatedAt \
        \WHERE projectId = :projectId"
        [":projectId" := projectId, ":owner" := owner, ":updatedAt" := now]
    result :: [Project] <- liftIO $ query conn
      "SELECT projectId, title, description, owner, createdBy, createdAt, updatedAt \
      \FROM projects WHERE projectId = ?"
      (Only projectId)
    case result of
      [p] -> return $ next p 
      _   -> throwError $ err404 { errBody = "project not found" }

  -- issues
  CreateIssue projectId next -> do
    issueId <- liftIO genObjectId
    now <- liftIO getCurrentTime
    let issue =
          Issue
          { issueId
          , parentId = Nothing
          , projectId
          , title = ""
          , description = ""
          , owner = _userId user 
          , assignee = Nothing
          , state = ""
          , createdBy = _userId user
          , createdAt = now
          , updatedAt = now
          }
    liftIO $ log DEBUG (toLogStr $ "CREATE issue: " <> show issueId)
    liftIO $ execute conn
      "INSERT INTO issues (issueId, parentId, projectId, title, description, owner, assignee, \
                          \state, createdBy, createdAt, updatedAt) \
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      issue 
    return $ next issue

  DeleteIssue issueId next -> do
    liftIO $ log DEBUG (toLogStr $ "DELETE issue: " <> show issueId)
    liftIO $ execute conn "DELETE FROM issues WHERE issudId = ?" (Only issueId)
    return $ next ()

  GetIssue issueId next -> do
    liftIO $ log DEBUG (toLogStr $ "GET issue: " <> show issueId)
    result :: [Issue] <- liftIO $ query conn "SELECT * FROM issues WHERE issueId = ?" (Only issueId)
    case result of
      [i] -> return $ next i
      _ -> throwError err404

  GetIssues projectIdM next -> do
    liftIO $ log DEBUG "GET issues"
    issues :: [Issue] <- liftIO $ 
      case projectIdM of
        Just projectId -> query conn "SELECT * FROM issues WHERE projectId = ?" (Only projectId)
        Nothing -> query_ conn "SELECT * FROM issues"
    return $ next issues

  UpdateIssue issueId updates next -> do
    liftIO $ log DEBUG (toLogStr $ "UDPATE issue: " <> show issueId)
    now <- liftIO getCurrentTime
    liftIO $ forM_ updates $ \case
      Title title -> executeNamed conn
        "UPDATE issues \
        \SET title = :title, updatedAt = :updatedAt \
        \WHERE issueId = :issueId"
        [":issueId" := issueId, ":updatedAt" := now, ":title" := title]
      Description description -> executeNamed conn
        "UPDATE issues \
        \SET description = :description, updatedAt = :updatedAt \
        \WHERE issueId = :issueId"
        [":issueId" := issueId, ":updatedAt" := now, ":description" := description]
      Owner owner -> executeNamed conn
        "UPDATE issues \
        \SET owner = :owner, updatedAt = :updatedAt \
        \WHERE issueId = :issueId"
        [":issueId" := issueId, ":updatedAt" := now, ":owner" := owner]
      Assignee assignee -> executeNamed conn
        "UPDATE issues \
        \SET assignee = :assignee, updatedAt = :updatedAt \
        \WHERE issueId = :issueId"
        [":issueId" := issueId, ":updatedAt" := now, ":assignee" := assignee]
      State state -> executeNamed conn
        "UPDATE issues \
        \SET state = :state, updatedAt = :updatedAt \
        \WHERE issueId = :issueId"
        [":issueId" := issueId, ":updatedAt" := now, ":state" := state]
    issue :: [Issue] <- liftIO $ query conn "SELECT * FROM issues WHERE issueId = ?" (Only issueId)
    case issue of
      [i] -> return $ next i
      _ -> throwError err404

  -- comments 
  CreateComment issueId next -> do
    commentId <- liftIO genObjectId
    now <- liftIO getCurrentTime
    let _comment =
          Comment
          { commentId
          , issueId
          , content = ""
          , createdBy = _userId user
          , createdAt = now
          , updatedAt = now
          }
    liftIO $ log DEBUG (toLogStr $ "CREATE comment: " <> show commentId)
    return $ next $ error "not implemented yet" -- TODO
    
  DeleteComment commentId next -> do
    liftIO $ log DEBUG (toLogStr $ "DELETE comment: " <> show commentId)
    return $ next $ error "not implemented yet" -- TODO

  GetComments Nothing _ -> throwError $ err400 { errBody = "issueId required" }

  GetComments (Just issueId) next -> do
    liftIO $ log DEBUG (toLogStr $ "GET comments: " <> show issueId)
    return $ next $ error "not implemented yet" -- TODO

  UpdateComment commentId _updates next -> do
    liftIO $ log DEBUG (toLogStr $ "UPDATE comment: " <> show commentId)
    _now <- liftIO getCurrentTime
    return $ next $ error "not implemented yet" -- TODO

  Log _msg                     _next -> error "not implemented"
  Err _e                             -> error "not implemented"

makeRunTime :: Connection -> RunTime
makeRunTime conn = RunTime (\logger user -> foldFree (interpret conn logger user))

withRunTime :: FilePath -> (RunTime -> IO a) -> IO a
withRunTime databaseFile program = withConnection databaseFile $ \conn -> do
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS projects (\
    \  projectId TEXT PRIMARY KEY, \
    \  title TEXT, \
    \  description TEXT, \
    \  owner TEXT, \
    \  createdBy TEXT, \
    \  createdAt TEXT, \
    \  updatedAt TEXT \
    \)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS issues (\
    \ issueId TEXT PRIMARY KEY, \
    \ parentId TEXT, \
    \ projectId TEXT, \
    \ title TEXT, \
    \ description TEXT, \
    \ owner TEXT, \
    \ assignee TEXT, \
    \ state TEXT, \
    \ createdBy TEXT, \
    \ createdAt TEXT, \
    \ updatedAt TEXT \
    \)"
  -- TODO create comments table
  program $ makeRunTime conn 


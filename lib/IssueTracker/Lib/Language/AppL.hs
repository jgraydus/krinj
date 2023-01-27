module IssueTracker.Lib.Language.AppL where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Exception (SomeException)
import Control.Monad.Free (Free)
import IssueTracker.Lib.Model
import Servant.Server (ServerError)

data AppL a where
  -- projects
  CreateProject :: (Project -> a) -> AppL a
  DeleteProject :: ProjectId -> (() -> a) -> AppL a
  GetProject :: ProjectId -> (Project -> a) -> AppL a
  GetProjects :: ([Project] -> a) -> AppL a
  UpdateProject :: ProjectId -> [ProjectUpdate] -> (Project -> a) -> AppL a
  -- issues
  CreateIssue :: ProjectId -> (Issue -> a) -> AppL a
  DeleteIssue :: IssueId -> (() -> a) -> AppL a
  GetIssue :: IssueId -> (Issue -> a) -> AppL a
  GetIssues :: Maybe ProjectId -> ([Issue] -> a) -> AppL a
  UpdateIssue :: IssueId -> [IssueUpdate] -> (Issue -> a) -> AppL a
  -- comments
  CreateComment :: IssueId -> (Comment -> a) -> AppL a
  DeleteComment :: CommentId -> (() -> a) -> AppL a
  GetComments :: Maybe IssueId -> ([Comment] -> a) -> AppL a
  UpdateComment :: CommentId -> [CommentUpdate] -> (Comment -> a) -> AppL a
  -- misc
  Log :: String -> (() -> a) -> AppL a
  Err :: SomeException -> AppL a
  
deriving instance Functor AppL

type App = Free AppL

type Interpreter = forall m a. (MonadIO m, MonadError ServerError m) => User -> App a -> m a


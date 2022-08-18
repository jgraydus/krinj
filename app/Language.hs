module Language where

import Control.Exception (SomeException)
import Control.Monad.Free (foldFree, Free, liftF)
import Model
import ObjectId

data AppL a where
  CreateIssue :: (Issue -> a) -> AppL a
  DeleteIssue :: IssueId -> (() -> a) -> AppL a
  GetIssue :: IssueId -> (Issue -> a) -> AppL a
  UpdateIssue :: IssueId -> [IssueUpdate] -> (Issue -> a) -> AppL a
  Log :: String -> (() -> a) -> AppL a
  Err :: SomeException -> AppL a
  
deriving instance Functor AppL

type App = Free AppL

createIssue :: App Issue
createIssue = liftF $ CreateIssue id

deleteIssue :: IssueId -> App ()
deleteIssue issueId = liftF $ DeleteIssue issueId id

getIssue :: IssueId -> App Issue
getIssue issueId = liftF $ GetIssue issueId id

updateIssue :: IssueId -> [IssueUpdate] -> App Issue
updateIssue issueId updates = liftF $ UpdateIssue issueId updates id

log :: String -> App ()
log msg = liftF $ Log msg id

err :: SomeException -> App a
err e = liftF $ Err e


interpret :: AppL a -> IO a
interpret = \case
  CreateIssue                 next -> error "not implemented"
  DeleteIssue issueId         next -> error "not implemented"
  GetIssue issueId            next -> error "not implemented"
  UpdateIssue issueId updates next -> error "not implemented"
  Log msg                     next -> error "not implemented"
  Err e                            -> error "not implemented"

runApp :: App a -> IO a
runApp = foldFree interpret


module Language where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Exception (SomeException)
import Control.Monad.Free (foldFree, Free, liftF)
import Data.Bson (genObjectId)
import Data.UUID (UUID)
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

interpret :: MonadIO m => User -> AppL a -> m a
interpret user = \case

  CreateIssue                 next -> do
    -- TODO implement
    issueId <- liftIO $ genObjectId
    let issue = Issue { issueId }
    liftIO $ putStrLn ("CREATE: " <> show issue)
    return $ next issue
    
  DeleteIssue issueId         next -> do
    -- TODO implement
    liftIO $ putStrLn ("DELETE: " <> show issueId)
    return $ next ()

  GetIssue issueId            next -> do
    -- TODO implement
    liftIO $ putStrLn ("GET: " <> show issueId)
    return $ next $ Issue { issueId }

  UpdateIssue issueId updates next -> do
    -- TODO implement
    liftIO $ putStrLn ("UDPATE: " <> show issueId)
    return $ next $ Issue { issueId }

  Log msg                     next -> error "not implemented"
  Err e                            -> error "not implemented"

runApp :: MonadIO m => User -> App a -> m a
runApp user = foldFree (interpret user)


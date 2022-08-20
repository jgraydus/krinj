module Language where

import Prelude hiding (lookup)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Except (MonadError)
import Control.Exception (SomeException)
import Control.Monad.Free (foldFree, Free, liftF)
import Data.Bson ((=:), Document, genObjectId, lookup, merge, Value(UTC))
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID)
import GHC.Conc (atomically)
import Model
import ObjectId
import Servant (throwError)
import Servant.Server (err404, err500, errBody, ServerError)
import qualified StmContainers.Map as StmMap

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

type DB = StmMap.Map ObjectId Document

isDeleted :: Document -> Bool
isDeleted doc = fromMaybe False $ lookup "isDeleted" doc

interpret :: (MonadIO m, MonadError ServerError m) => DB -> User -> AppL a -> m a
interpret db user = \case

  CreateIssue next -> do
    issueId <- liftIO genObjectId
    now <- liftIO getCurrentTime
    let issue =
          Issue
          { issueId
          , title = ""
          , description = ""
          , owner = _userId user 
          , assignee = Nothing
          , state = ""
          , createdBy = _userId user
          , createdAt = now
          , updatedAt = now
          }
    liftIO $ putStrLn ("CREATE: " <> show issueId)
    let doc = issueToDocument issue <> ["isDeleted" =: False]
    liftIO $ atomically $ StmMap.insert doc issueId db
    return $ next issue
    
  DeleteIssue issueId next -> do
    liftIO $ putStrLn ("DELETE: " <> show issueId)
    errMaybe <- liftIO $ atomically $ do
      docMaybe <- StmMap.lookup issueId db
      case docMaybe of
        Just doc -> do
          StmMap.insert (doc <> ["isDeleted" =: True]) issueId db
          return Nothing
        Nothing -> return $ Just err404
    case errMaybe of
      Just err -> throwError err
      Nothing -> return $ next () 

  GetIssue issueId next -> do
    liftIO $ putStrLn ("GET: " <> show issueId)
    docMaybe <- liftIO $ atomically $ StmMap.lookup issueId db
    case docMaybe of
      Just doc | not (isDeleted doc)-> do
        case documentToIssue doc of
          Right issue -> return $ next issue 
          Left err    -> throwError $ err500 { errBody = (fromStrict . encodeUtf8) err }
      _ -> throwError err404

  UpdateIssue issueId updates next -> do
    liftIO $ putStrLn ("UDPATE: " <> show issueId)
    now <- liftIO getCurrentTime
    result <- liftIO $ atomically $ do
      docMaybe <- StmMap.lookup issueId db
      case docMaybe of
        Just doc -> do
          let updatedDoc = merge (issueUpdatesToDocument updates <> ["updatedAt" =: UTC now]) doc
          StmMap.insert updatedDoc issueId db
          return $ Right updatedDoc
        Nothing  -> return $ Left err404
    case result of
      Left err -> throwError err
      Right doc -> case documentToIssue doc of
        Right issue -> return $ next issue
        Left err -> throwError $ err500 { errBody = (fromStrict . encodeUtf8) err }

  Log msg                     next -> error "not implemented"
  Err e                            -> error "not implemented"

runApp :: (MonadIO m, MonadError ServerError m) => DB -> User -> App a -> m a
runApp db user = foldFree (interpret db user)


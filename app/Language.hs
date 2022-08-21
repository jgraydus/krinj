module Language where

import           Prelude hiding (lookup)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Except (MonadError)
import           Control.Exception (SomeException)
import           Control.Monad.Free (foldFree, Free, liftF)
import           Data.Bson ((=:), at, Document, genObjectId, lookup, merge, Value(UTC))
import           Data.ByteString.Lazy (fromStrict)
import           Data.Maybe (fromMaybe)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Clock (getCurrentTime)
import           Data.UUID (UUID)
import           GHC.Conc (atomically)
import qualified ListT as ListT
import           Model
import           ObjectId
import           Servant (throwError)
import           Servant.Server (err400, err404, err500, errBody, ServerError)
import qualified StmContainers.Map as StmMap

data AppL a where
  -- projects
  CreateProject :: (Project -> a) -> AppL a
  DeleteProject :: ProjectId -> (() -> a) -> AppL a
  GetProject :: ProjectId -> (Project -> a) -> AppL a
  UpdateProject :: ProjectId -> [ProjectUpdate] -> (Project -> a) -> AppL a
  -- issues
  CreateIssue :: ProjectId -> (Issue -> a) -> AppL a
  DeleteIssue :: IssueId -> (() -> a) -> AppL a
  GetIssue :: IssueId -> (Issue -> a) -> AppL a
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

-- projects
createProject :: App Project
createProject = liftF $ CreateProject id

deleteProject :: ProjectId -> App ()
deleteProject projectId = liftF $ DeleteProject projectId id

getProject :: ProjectId -> App Project
getProject projectId = liftF $ GetProject projectId id

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

data DB = DB
  { _projects :: StmMap.Map ObjectId Document
  , _issues :: StmMap.Map ObjectId Document
  , _comments :: StmMap.Map ObjectId Document
  }

isDeleted :: Document -> Bool
isDeleted doc = fromMaybe False $ lookup "isDeleted" doc

values :: MonadIO m => StmMap.Map k v -> m [v]
values = liftIO . atomically . (fmap . fmap) snd . ListT.toList . StmMap.listT

interpret :: (MonadIO m, MonadError ServerError m) => DB -> User -> AppL a -> m a
interpret db user = \case
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
    liftIO $ putStrLn ("CREATE project: " <> show projectId)
    let doc = projectToDocument project <> ["isDeleted" =: False]
    liftIO $ atomically $ StmMap.insert doc projectId (_projects db)
    return $ next project

  DeleteProject projectId next -> do
    liftIO $ putStrLn ("DELETE project: " <> show projectId)
    errMaybe <- liftIO $ atomically $ do
      docMaybe <- StmMap.lookup projectId (_projects db)
      case docMaybe of
        Just doc -> do
          StmMap.insert (doc <> ["isDeleted" =: True]) projectId (_projects db)
          return Nothing
        Nothing -> return $ Just err404
    case errMaybe of
      Just err -> throwError err
      Nothing -> return $ next ()

  GetProject projectId next -> do
    liftIO $ putStrLn ("GET project: " <> show projectId)
    docMaybe <- liftIO $ atomically $ StmMap.lookup projectId (_projects db)
    case docMaybe of
      Just doc | not (isDeleted doc)-> do
        case documentToProject doc of
          Right project -> return $ next project
          Left err    -> throwError $ err500 { errBody = (fromStrict . encodeUtf8) err }
      _ -> throwError err404

  UpdateProject projectId updates next -> do
    liftIO $ putStrLn ("UDPATE project: " <> show projectId)
    now <- liftIO getCurrentTime
    result <- liftIO $ atomically $ do
      docMaybe <- StmMap.lookup projectId (_projects db)
      case docMaybe of
        Just doc -> do
          let updatedDoc = merge (projectUpdatesToDocument updates <> ["updatedAt" =: UTC now]) doc
          StmMap.insert updatedDoc projectId (_projects db)
          return $ Right updatedDoc
        Nothing  -> return $ Left err404
    case result of
      Left err -> throwError err
      Right doc -> case documentToProject doc of
        Right issue -> return $ next issue
        Left err -> throwError $ err500 { errBody = (fromStrict . encodeUtf8) err }

  -- issues
  CreateIssue projectId next -> do
    issueId <- liftIO genObjectId
    now <- liftIO getCurrentTime
    let issue =
          Issue
          { issueId
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
    liftIO $ putStrLn ("CREATE issue: " <> show issueId)
    let doc = issueToDocument issue <> ["isDeleted" =: False]
    liftIO $ atomically $ StmMap.insert doc issueId (_issues db)
    return $ next issue
    
  DeleteIssue issueId next -> do
    liftIO $ putStrLn ("DELETE issue: " <> show issueId)
    errMaybe <- liftIO $ atomically $ do
      docMaybe <- StmMap.lookup issueId (_issues db)
      case docMaybe of
        Just doc -> do
          StmMap.insert (doc <> ["isDeleted" =: True]) issueId (_issues db)
          return Nothing
        Nothing -> return $ Just err404
    case errMaybe of
      Just err -> throwError err
      Nothing -> return $ next () 

  GetIssue issueId next -> do
    liftIO $ putStrLn ("GET issue: " <> show issueId)
    docMaybe <- liftIO $ atomically $ StmMap.lookup issueId (_issues db)
    case docMaybe of
      Just doc | not (isDeleted doc)-> do
        case documentToIssue doc of
          Right issue -> return $ next issue 
          Left err    -> throwError $ err500 { errBody = (fromStrict . encodeUtf8) err }
      _ -> throwError err404

  UpdateIssue issueId updates next -> do
    liftIO $ putStrLn ("UDPATE issue: " <> show issueId)
    now <- liftIO getCurrentTime
    result <- liftIO $ atomically $ do
      docMaybe <- StmMap.lookup issueId (_issues db)
      case docMaybe of
        Just doc -> do
          let updatedDoc = merge (issueUpdatesToDocument updates <> ["updatedAt" =: UTC now]) doc
          StmMap.insert updatedDoc issueId (_issues db)
          return $ Right updatedDoc
        Nothing  -> return $ Left err404
    case result of
      Left err -> throwError err
      Right doc -> case documentToIssue doc of
        Right issue -> return $ next issue
        Left err -> throwError $ err500 { errBody = (fromStrict . encodeUtf8) err }

  -- comments 
  CreateComment issueId next -> do
    commentId <- liftIO genObjectId
    now <- liftIO getCurrentTime
    let comment =
          Comment
          { commentId
          , issueId
          , content = ""
          , createdBy = _userId user
          , createdAt = now
          , updatedAt = now
          }
    liftIO $ putStrLn ("CREATE comment: " <> show commentId)
    let doc = commentToDocument comment <> ["isDeleted" =: False]
    liftIO $ atomically $ StmMap.insert doc commentId (_comments db)
    return $ next comment
    
  DeleteComment commentId next -> do
    liftIO $ putStrLn ("DELETE comment: " <> show commentId)
    errMaybe <- liftIO $ atomically $ do
      docMaybe <- StmMap.lookup commentId (_comments db)
      case docMaybe of
        Just doc -> do
          StmMap.insert (doc <> ["isDeleted" =: True]) commentId (_comments db)
          return Nothing
        Nothing -> return $ Just err404
    case errMaybe of
      Just err -> throwError err
      Nothing -> return $ next () 

  GetComments Nothing _ -> throwError $ err400 { errBody = (fromStrict . encodeUtf8) "issueId required" }

  GetComments (Just issueId) next -> do
    liftIO $ putStrLn ("GET comments: " <> show issueId)
    allComments <- values (_comments db)
    let conditions doc = not (isDeleted doc) && at "issueId" doc == issueId
        comments :: [Document] = filter conditions allComments
    case traverse documentToComment comments of
      Right cs -> return $ next cs
      Left err -> throwError $ err500 { errBody = (fromStrict . encodeUtf8) err }

  UpdateComment commentId updates next -> do
    liftIO $ putStrLn ("UPDATE comment: " <> show commentId)
    now <- liftIO getCurrentTime
    result <- liftIO $ atomically $ do
      docMaybe <- StmMap.lookup commentId (_comments db)
      case docMaybe of
        Just doc -> do
          let updatedDoc = merge (commentUpdatesToDocument updates <> ["updatedAt" =: UTC now]) doc
          StmMap.insert updatedDoc commentId (_comments db)
          return $ Right updatedDoc
        Nothing  -> return $ Left err404
    case result of
      Left err -> throwError err
      Right doc -> case documentToComment doc of
        Right comment -> return $ next comment
        Left err -> throwError $ err500 { errBody = (fromStrict . encodeUtf8) err }

  Log msg                     next -> error "not implemented"
  Err e                            -> error "not implemented"

runApp :: (MonadIO m, MonadError ServerError m) => DB -> User -> App a -> m a
runApp db user = foldFree (interpret db user)


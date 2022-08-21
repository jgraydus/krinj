module Issues.Lib.Language.Interpreter.InMemory where

import           Prelude hiding (lookup)

import           Control.Monad.Except (MonadError)
import           Control.Monad.Free (foldFree)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Bson ((=:), at, Document, genObjectId, lookup, merge, Value(UTC))
import           Data.ByteString.Lazy (fromStrict)
import           Data.Maybe (fromMaybe)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Clock (getCurrentTime)
import qualified ListT as ListT
import           GHC.Conc (atomically)
import qualified StmContainers.Map as StmMap
import           Servant (throwError)
import           Servant.Server (err400, err404, err500, errBody, ServerError)

import           Issues.Lib.Language.AppL
import           Issues.Lib.Model
import           Issues.Lib.Language.RunTime
import           Issues.Lib.ObjectId

data DB = DB
  { _projects :: StmMap.Map ObjectId Document
  , _issues :: StmMap.Map ObjectId Document
  , _comments :: StmMap.Map ObjectId Document
  }

newDB :: IO DB
newDB = DB <$> StmMap.newIO <*> StmMap.newIO <*> StmMap.newIO

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
      Just e -> throwError e
      Nothing -> return $ next ()

  GetProject projectId next -> do
    liftIO $ putStrLn ("GET project: " <> show projectId)
    docMaybe <- liftIO $ atomically $ StmMap.lookup projectId (_projects db)
    case docMaybe of
      Just doc | not (isDeleted doc)-> do
        case documentToProject doc of
          Right project -> return $ next project
          Left e    -> throwError $ err500 { errBody = (fromStrict . encodeUtf8) e }
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
      Left e -> throwError e
      Right doc -> case documentToProject doc of
        Right issue -> return $ next issue
        Left e -> throwError $ err500 { errBody = (fromStrict . encodeUtf8) e }

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
      Just e -> throwError e
      Nothing -> return $ next () 

  GetIssue issueId next -> do
    liftIO $ putStrLn ("GET issue: " <> show issueId)
    docMaybe <- liftIO $ atomically $ StmMap.lookup issueId (_issues db)
    case docMaybe of
      Just doc | not (isDeleted doc)-> do
        case documentToIssue doc of
          Right issue -> return $ next issue 
          Left e      -> throwError $ err500 { errBody = (fromStrict . encodeUtf8) e }
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
      Left e -> throwError e
      Right doc -> case documentToIssue doc of
        Right issue -> return $ next issue
        Left e -> throwError $ err500 { errBody = (fromStrict . encodeUtf8) e }

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
      Just e -> throwError e
      Nothing -> return $ next () 

  GetComments Nothing _ -> throwError $ err400 { errBody = (fromStrict . encodeUtf8) "issueId required" }

  GetComments (Just issueId) next -> do
    liftIO $ putStrLn ("GET comments: " <> show issueId)
    allComments <- values (_comments db)
    let conditions doc = not (isDeleted doc) && at "issueId" doc == issueId
        comments :: [Document] = filter conditions allComments
    case traverse documentToComment comments of
      Right cs -> return $ next cs
      Left e -> throwError $ err500 { errBody = (fromStrict . encodeUtf8) e }

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
      Left e -> throwError e
      Right doc -> case documentToComment doc of
        Right comment -> return $ next comment
        Left e -> throwError $ err500 { errBody = (fromStrict . encodeUtf8) e }

  Log _msg                     _next -> error "not implemented"
  Err _e                             -> error "not implemented"

makeRunTime :: IO RunTime
makeRunTime = do
  db <- newDB
  return $ RunTime (\user -> foldFree (interpret db user))


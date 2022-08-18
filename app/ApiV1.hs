module ApiV1 where

import Auth
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Bson (genObjectId)
import Data.Text (pack, Text)
import GHC.Generics
import Network.Wai (Request)
import ObjectId
import Servant
import Servant.API.Experimental.Auth
import Servant.Server
import Servant.Server.Experimental.Auth

type IssueId = ObjectId

data Issue = Issue
  { issueId :: IssueId 
  } deriving (Generic, Show)

instance ToJSON Issue

------------------------------------------

type CreateIssue = "create" :> Post '[JSON] Issue

createIssue :: Handler Issue
createIssue = do
  -- TODO implement
  issueId <- liftIO $ genObjectId
  let issue = Issue { issueId }
  liftIO $ putStrLn ("CREATE: " <> show issue)
  return issue

------------------------------------------

type DeleteIssue = "delete" :> Capture "issueId" IssueId :> Delete '[JSON] ()

deleteIssue :: IssueId -> Handler ()
deleteIssue issueId = do
  -- TODO implement
  liftIO $ putStrLn ("DELETE: " <> show issueId)
  return ()

------------------------------------------

type GetIssue = Capture "issueId" IssueId :> Get '[JSON] Issue

getIssue :: IssueId -> Handler Issue
getIssue issueId = do
  -- TODO implement
  liftIO $ putStrLn ("GET: " <> show issueId)
  return $ Issue { issueId }

------------------------------------------


type API_V1 = JwtAuth :> "v1" :> "issues" :> (CreateIssue :<|> DeleteIssue :<|> GetIssue)

apiV1Server :: Server API_V1
apiV1Server user = createIssue :<|> deleteIssue :<|> getIssue


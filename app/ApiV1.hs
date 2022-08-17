module ApiV1 where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Bson (genObjectId)
import Data.Text (pack, Text)
import GHC.Generics
import ObjectId
import Servant
import Servant.Server

type IssueId = ObjectId

data Issue = Issue
  { issueId :: IssueId 
  } deriving (Generic, Show)

instance ToJSON Issue

------------------------------------------

type CreateIssue = "v1" :> "issues" :> "create" :> Post '[JSON] Issue

createIssue :: Handler Issue
createIssue = do
  issueId <- liftIO $ genObjectId
  let issue = Issue { issueId }
  liftIO $ putStrLn ("CREATE: " <> show issue)
  return issue

------------------------------------------

type DeleteIssue = "v1" :> "issues" :> "delete" :> Capture "issueId" IssueId :> Delete '[JSON] ()

deleteIssue :: IssueId -> Handler ()
deleteIssue issueId = do
  -- TODO implement
  liftIO $ putStrLn ("DELETE: " <> show issueId)
  return ()



------------------------------------------

type API_V1 = CreateIssue :<|> DeleteIssue

apiV1Server :: Server API_V1
apiV1Server = createIssue :<|> deleteIssue


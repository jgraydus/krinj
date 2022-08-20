module ApiV1 where

import Auth
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Bson (genObjectId)
import Data.Text (pack, Text)
import qualified Language as L
import Model
import Network.Wai (Request)
import ObjectId
import Servant
import Servant.API.Experimental.Auth
import Servant.Server
import Servant.Server.Experimental.Auth

------------------------------------------
-- Create issue

type CreateIssue = "create" :> Post '[JSON] Issue

createIssue :: User -> Handler Issue
createIssue user = L.runApp user L.createIssue

------------------------------------------
-- Delete issue

type DeleteIssue = "delete" :> Capture "issueId" IssueId :> Delete '[JSON] ()

deleteIssue :: User -> IssueId -> Handler ()
deleteIssue user issueId = L.runApp user (L.deleteIssue issueId)

------------------------------------------
-- Get issue

type GetIssue = Capture "issueId" IssueId :> Get '[JSON] Issue

getIssue :: User -> IssueId -> Handler Issue
getIssue user issueId = L.runApp user (L.getIssue issueId)

------------------------------------------
-- Update issue

type UpdateIssue = Capture "issueId" IssueId :> ReqBody '[JSON] [IssueUpdate] :> Patch '[JSON] Issue

updateIssue :: User -> IssueId -> [IssueUpdate] -> Handler Issue
updateIssue user issueId issueUpdates = L.runApp user (L.updateIssue issueId issueUpdates)

------------------------------------------
-- API definition and composite handler

type API_V1 = JwtAuth :> "v1" :> "issues" :> (CreateIssue :<|> DeleteIssue :<|> GetIssue :<|> UpdateIssue)

apiV1Server :: Server API_V1
apiV1Server user =
  createIssue user
  :<|> deleteIssue user
  :<|> getIssue user
  :<|> updateIssue user


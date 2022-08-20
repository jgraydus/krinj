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

createIssue :: L.DB -> User -> Handler Issue
createIssue db user = L.runApp db user L.createIssue

------------------------------------------
-- Delete issue

type DeleteIssue = "delete" :> Capture "issueId" IssueId :> Delete '[JSON] ()

deleteIssue :: L.DB -> User -> IssueId -> Handler ()
deleteIssue db user issueId = L.runApp db user (L.deleteIssue issueId)

------------------------------------------
-- Get issue

type GetIssue = Capture "issueId" IssueId :> Get '[JSON] Issue

getIssue :: L.DB -> User -> IssueId -> Handler Issue
getIssue db user issueId = L.runApp db user (L.getIssue issueId)

------------------------------------------
-- Update issue

type UpdateIssue = Capture "issueId" IssueId :> ReqBody '[JSON] [IssueUpdate] :> Patch '[JSON] Issue

updateIssue :: L.DB -> User -> IssueId -> [IssueUpdate] -> Handler Issue
updateIssue db user issueId issueUpdates = L.runApp db user (L.updateIssue issueId issueUpdates)

------------------------------------------
-- API definition and composite handler

type API_V1 = JwtAuth :> "v1" :> "issues" :> (CreateIssue :<|> DeleteIssue :<|> GetIssue :<|> UpdateIssue)

apiV1Server :: L.DB -> Server API_V1
apiV1Server db user =
  createIssue db user
  :<|> deleteIssue db user
  :<|> getIssue db user
  :<|> updateIssue db user


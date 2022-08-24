module Issues.Lib.Model.Issue (
    documentToIssue,
    Issue(..),
    IssueId,
    issueToDocument,
    IssueUpdate(..),
    issueUpdatesToDocument
) where

import           Prelude hiding (String)

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Bson ((=:), Document, Field, Value(..))
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.UUID (UUID)
import           GHC.Generics (Generic)

import           Issues.Lib.Model.Project (ProjectId)
import           Issues.Lib.Model.Util (fromBsonUuid, lookup', toBsonUuid)
import           Issues.Lib.ObjectId

type IssueId = ObjectId

data Issue = Issue
  { issueId :: IssueId 
  , projectId :: ProjectId
  , title :: Text
  , description :: Text
  , owner :: UUID
  , assignee :: Maybe UUID
  , state :: Text
  , createdBy :: UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  } deriving (Generic, Show)

instance ToJSON Issue

issueToDocument :: Issue -> Document
issueToDocument Issue {..} =
  [ "issueId" =: ObjId issueId
  , "projectId" =: ObjId projectId
  , "title" =: String title
  , "description" =: String description
  , "owner" =: (Uuid . toBsonUuid) owner
  , "assignee" =: (fromMaybe Null $ (Uuid . toBsonUuid) <$> assignee)
  , "state" =: state
  , "createdBy" =: (Uuid . toBsonUuid) createdBy
  , "createdAt" =: UTC createdAt
  , "updatedAt" =: UTC updatedAt
  ]

documentToIssue :: Document -> Either Text Issue
documentToIssue doc =
  Issue
  <$> lookup' "issueId" doc
  <*> lookup' "projectId" doc
  <*> lookup' "title" doc
  <*> lookup' "description" doc
  <*> (lookup' "owner" doc >>= fromBsonUuid)
  <*> (lookup' "assignee" doc >>= (\case Nothing -> Right Nothing; Just x -> Just <$> fromBsonUuid x))
  <*> lookup' "state" doc
  <*> (lookup' "createdBy" doc >>= fromBsonUuid)
  <*> lookup' "createdAt" doc
  <*> lookup' "updatedAt" doc

data IssueUpdate =
    Title Text
  | Description Text
  | Owner UUID
  | Assignee (Maybe UUID)
  | State Text
  deriving (Generic, Show)

instance ToJSON IssueUpdate
instance FromJSON IssueUpdate

issueUpdateToField :: IssueUpdate -> Field
issueUpdateToField = \case
  Title txt -> "title" =: String txt
  Description desc -> "description" =: String desc
  Owner uuid -> "owner" =: (Uuid . toBsonUuid) uuid
  Assignee ass -> "assignee" =: (fromMaybe Null $ (Uuid . toBsonUuid) <$> ass)
  State state -> "state" =: String state

issueUpdatesToDocument :: [IssueUpdate] -> Document
issueUpdatesToDocument = fmap issueUpdateToField


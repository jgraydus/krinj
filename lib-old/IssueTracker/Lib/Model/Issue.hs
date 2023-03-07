module IssueTracker.Lib.Model.Issue (
    documentToIssue,
    Issue(..),
    IssueId,
    issueToDocument,
    IssueUpdate(..),
    issueUpdatesToDocument
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bson ((=:), Document, Field, Value(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Database.SQLite.Simple.FromRow (FromRow)
import Database.SQLite.Simple.ToRow (ToRow)
import GHC.Generics (Generic)
import IssueTracker.Lib.Model.Project (ProjectId)
import IssueTracker.Lib.Model.Util (fromBsonUuid, lookup', toBsonUuid)
import IssueTracker.Lib.ObjectId (ObjectId)
import Prelude hiding (String)

type IssueId = ObjectId

data Issue = Issue
  { issueId :: IssueId 
  , parentId :: Maybe IssueId
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
instance FromRow Issue
instance ToRow Issue

issueToDocument :: Issue -> Document
issueToDocument Issue {..} =
  [ "_id" =: ObjId issueId
  , "parentId" =: fromMaybe Null (ObjId <$> parentId)
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
  <$> lookup' "_id" doc
  <*> (lookup' "parentId" doc :: Either Text (Maybe IssueId))
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


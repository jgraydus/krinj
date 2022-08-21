module Issues.Lib.Model where

import           Prelude hiding (lookup, String)
import           Data.Aeson hiding (Null, String)
import           Data.Bson ((=:), Document, Field, lookup, Val, Value(..))
import qualified Data.Bson as Bson
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.UUID
import           Data.UUID.V4 (nextRandom)
import           GHC.Generics
import           Issues.Lib.ObjectId

toBsonUuid :: UUID -> Bson.UUID
toBsonUuid = Bson.UUID . toStrict . toByteString

fromBsonUuid :: Bson.UUID -> Either Text UUID
fromBsonUuid (Bson.UUID bs) =
  case (fromByteString . fromStrict) bs of
    Nothing -> Left "failed to convert UUID"
    Just x -> Right x

lookup' :: Val a => Text -> Document -> Either Text a
lookup' label doc = case lookup label doc of
  Nothing -> Left $ "failed to find field [" <> label <> "]"
  Just x  -> Right x

--------------------------------------------------
data User = User
  { _userId :: UUID
  } deriving (Generic, Show)

instance FromJSON User
instance ToJSON User

makeUserForTest :: IO User
makeUserForTest = do
  _userId <- nextRandom
  return $ User {..}

--------------------------------------------------
type ProjectId = ObjectId

data Project = Project
  { projectId :: ProjectId
  , title :: Text
  , description :: Text
  , owner :: UUID
  , createdBy :: UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  } deriving (Generic, Show)

instance ToJSON Project

projectToDocument :: Project -> Document
projectToDocument Project {..} =
  [ "projectId" =: ObjId projectId
  , "title" =: String title
  , "description" =: String description
  , "owner" =: (Uuid . toBsonUuid) owner 
  , "createdBy" =: (Uuid . toBsonUuid) createdBy
  , "createdAt" =: UTC createdAt
  , "updatedAt" =: UTC updatedAt
  ]

documentToProject :: Document -> Either Text Project
documentToProject doc =
  Project
  <$> lookup' "projectId" doc
  <*> lookup' "title" doc
  <*> lookup' "description" doc
  <*> (lookup' "owner" doc >>= fromBsonUuid)
  <*> (lookup' "createdBy" doc >>= fromBsonUuid)
  <*> lookup' "createdAt" doc
  <*> lookup' "updatedAt" doc

data ProjectUpdate =
    ProjectTitle Text
  | ProjectDescription Text
  | ProjectOwner UUID
  deriving (Generic, Show)

instance FromJSON ProjectUpdate

projectUpdateToField :: ProjectUpdate -> Field
projectUpdateToField = \case
  ProjectTitle title -> "title" =: String title
  ProjectDescription desc -> "description" =: String desc
  ProjectOwner uuid -> "owner" =: (Uuid . toBsonUuid) uuid

projectUpdatesToDocument :: [ProjectUpdate] -> Document
projectUpdatesToDocument = fmap projectUpdateToField

--------------------------------------------------
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

--------------------------------------------------
type CommentId = ObjectId

data Comment = Comment
  { commentId :: CommentId
  , issueId :: IssueId
  , content :: Text
  , createdBy :: UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  } deriving (Generic, Show)

instance ToJSON Comment

commentToDocument :: Comment -> Document
commentToDocument Comment {..} =
  [ "commentId" =: ObjId commentId
  , "issueId" =: ObjId issueId
  , "content" =: String content
  , "createdBy" =: (Uuid . toBsonUuid) createdBy
  , "createdAt" =: UTC createdAt
  , "updatedAt" =: UTC updatedAt
  ]

documentToComment :: Document -> Either Text Comment
documentToComment doc =
  Comment
  <$> lookup' "commentId" doc
  <*> lookup' "issueId" doc
  <*> lookup' "content" doc
  <*> (lookup' "createdBy" doc >>= fromBsonUuid)
  <*> lookup' "createdAt" doc
  <*> lookup' "updatedAt" doc

data CommentUpdate =
    Content Text 
  | Dummy
  deriving (Generic, Show)

instance FromJSON CommentUpdate

commentUpdateToField :: CommentUpdate -> Field
commentUpdateToField = \case
  Content content -> "content" =: String content
  Dummy           -> error "do not use the Dummy constructor"

commentUpdatesToDocument :: [CommentUpdate] -> Document
commentUpdatesToDocument = fmap commentUpdateToField


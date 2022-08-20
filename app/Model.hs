module Model where

import           Prelude hiding (lookup, String)
import           Data.Aeson hiding (Null, String)
import           Data.AesonBson (bsonifyError)
import           Data.Bson ((=:), Document, lookup, Val, Value(..))
import qualified Data.Bson as Bson
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.UUID
import           GHC.Generics
import           ObjectId

--------------------------------------------------
data User = User
  { _userId :: UUID
  } deriving (Generic, Show)

instance FromJSON User
instance ToJSON User

--------------------------------------------------
type IssueId = ObjectId

data Issue = Issue
  { issueId :: IssueId 
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

data IssueUpdate = IssueUpdate
  deriving (Generic, Show)

instance FromJSON IssueUpdate

toBsonUuid :: UUID -> Bson.UUID
toBsonUuid = Bson.UUID . toStrict . toByteString

fromBsonUuid :: Bson.UUID -> Either Text UUID
fromBsonUuid (Bson.UUID bs) =
  case (fromByteString . fromStrict) bs of
    Nothing -> Left "failed to convert UUID"
    Just x -> Right x

issueToDocument :: Issue -> Document
issueToDocument Issue {..} =
  [ "issueId" =: ObjId issueId
  , "title" =: String title
  , "description" =: String description
  , "owner" =: (Uuid . toBsonUuid) owner
  , "assignee" =: (fromMaybe Null $ (Uuid . toBsonUuid) <$> assignee)
  , "state" =: state
  , "createdBy" =: (Uuid . toBsonUuid) createdBy
  , "createdAt" =: UTC createdAt
  , "updatedAt" =: UTC updatedAt
  ]

lookup' :: Val a => Text -> Document -> Either Text a
lookup' label doc = case lookup label doc of
  Nothing -> Left $ "failed to find field [" <> label <> "]"
  Just x  -> Right x

documentToIssue :: Document -> Either Text Issue
documentToIssue doc =
  Issue
  <$> lookup' "issueId" doc
  <*> lookup' "title" doc
  <*> lookup' "description" doc
  <*> (lookup' "owner" doc >>= fromBsonUuid)
  <*> (lookup' "assignee" doc >>= (\case Nothing -> Right Nothing; Just x -> Just <$> fromBsonUuid x))
  <*> lookup' "state" doc
  <*> (lookup' "createdBy" doc >>= fromBsonUuid)
  <*> lookup' "createdAt" doc
  <*> lookup' "updatedAt" doc


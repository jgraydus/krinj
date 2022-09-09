module IssueTracker.Lib.Model.Comment (
   documentToComment,
   Comment(..),
   CommentId,
   commentToDocument,
   CommentUpdate(..),
   commentUpdatesToDocument
) where

import           Prelude hiding (String)

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Bson ((=:), Document, Field, Value(..))
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.UUID (UUID)
import           Database.SQLite.Simple.FromRow (FromRow)
import           Database.SQLite.Simple.ToRow (ToRow)
import           GHC.Generics (Generic)

import           IssueTracker.Lib.Model.Issue (IssueId)
import           IssueTracker.Lib.Model.Util (fromBsonUuid, lookup', toBsonUuid)
import           IssueTracker.Lib.ObjectId

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
instance FromRow Comment
instance ToRow Comment

commentToDocument :: Comment -> Document
commentToDocument Comment {..} =
  [ "_id" =: ObjId commentId
  , "issueId" =: ObjId issueId
  , "content" =: String content
  , "createdBy" =: (Uuid . toBsonUuid) createdBy
  , "createdAt" =: UTC createdAt
  , "updatedAt" =: UTC updatedAt
  ]

documentToComment :: Document -> Either Text Comment
documentToComment doc =
  Comment
  <$> lookup' "_id" doc
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


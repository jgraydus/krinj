module Issues.Lib.Model.Comment (
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
import           GHC.Generics (Generic)

import           Issues.Lib.Model.Issue (IssueId)
import           Issues.Lib.Model.Util (fromBsonUuid, lookup', toBsonUuid)
import           Issues.Lib.ObjectId

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


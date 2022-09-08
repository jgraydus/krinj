module Issues.Lib.Model.Project (
   documentToProject,
   Project(..),
   ProjectId,
   projectToDocument,
   ProjectUpdate(..),
   projectUpdatesToDocument
) where

import           Prelude hiding (String)

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Bson ((=:), Document, Field, Value(..))
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.UUID (UUID)
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import           GHC.Generics (Generic)

import           Issues.Lib.Model.Util (fromBsonUuid, lookup', toBsonUuid)
import           Issues.Lib.ObjectId
import           Issues.Lib.UUID ()

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
instance FromRow Project
instance ToRow Project
 
projectToDocument :: Project -> Document
projectToDocument Project {..} =
  [ "_id" =: projectId
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
  <$> lookup' "_id" doc
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


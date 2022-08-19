module Model where

import Data.Aeson
import Data.UUID
import GHC.Generics
import ObjectId

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
  } deriving (Generic, Show)

instance ToJSON Issue

data IssueUpdate = IssueUpdate
  deriving (Generic, Show)

instance FromJSON IssueUpdate


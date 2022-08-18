module Model where

import Data.Aeson
import GHC.Generics
import ObjectId


data User = User

type IssueId = ObjectId

data Issue = Issue
  { issueId :: IssueId 
  } deriving (Generic, Show)

instance ToJSON Issue


data IssueUpdate = IssueUpdate
  deriving (Generic, Show)

instance FromJSON IssueUpdate





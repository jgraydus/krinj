module IssueTracker.Lib.Model.User (
    makeUserForTest,
    User(..)
) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.UUID (UUID)
import           Data.UUID.V4 (nextRandom)
import           GHC.Generics (Generic)

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


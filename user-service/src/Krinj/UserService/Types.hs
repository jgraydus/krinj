module Krinj.UserService.Types where

import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

type EmailAddress = Text
type Password = Text
type UserId = UUID

data User =
  User
  { userId :: UserId
  , emailAddress :: EmailAddress
  }
  deriving stock (Generic, Show)


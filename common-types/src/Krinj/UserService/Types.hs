module Krinj.UserService.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString(..))
import Data.Text (pack, Text, unpack)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Opaleye (DefaultFromField, SqlText)

type UserId = UUID

newtype EmailAddress = EmailAddress Text
  deriving stock (Generic)
  deriving newtype (DefaultFromField SqlText, Eq, FromJSON, Ord, ToJSON)

instance Show EmailAddress where
  show (EmailAddress t) = unpack t

instance IsString EmailAddress where
  fromString = EmailAddress . pack

newtype Password = Password Text
  deriving stock (Generic)
  deriving newtype (DefaultFromField SqlText, Eq, FromJSON, Ord, ToJSON)

instance Show Password where
  show = const "************"

instance IsString Password where
  fromString = Password . pack

newtype Salt = Salt Text
  deriving stock (Generic)
  deriving newtype (Eq, FromJSON, Ord, ToJSON)

instance Show Salt where
  show (Salt t) = unpack t

instance IsString Salt where
  fromString = Salt . pack

data User =
  User
  { userId :: UserId
  , emailAddress :: EmailAddress
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)


module Krinj.Config.Newtypes where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype HostName = HostName Text
  deriving stock (Generic)
  deriving newtype (Eq, FromJSON, Ord, Show, ToJSON)

newtype PortNumber = PortNumber Int
  deriving stock (Generic)
  deriving newtype (Eq, FromJSON, Ord, Show, ToJSON)

newtype Protocol = Protocol Text
  deriving stock (Generic)
  deriving newtype (Eq, FromJSON, Ord, Show, ToJSON)

newtype JwtKey = JwtKey Text
  deriving stock (Generic)
  deriving newtype (Eq, FromJSON, Ord, ToJSON)

instance Show JwtKey where
  show = const "************"

newtype Password = Password Text
  deriving stock (Generic)
  deriving newtype (Eq, FromJSON, Ord, ToJSON)

instance Show Password where
  show = const "************"


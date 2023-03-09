{-# LANGUAGE DerivingVia #-}
module EntityService.Internal.Model.Newtypes where

import Data.Aeson (FromJSON, ToJSON, ToJSONKey, Value)
import Data.Coerce (coerce)
import Data.Profunctor.Product.Default (Default(..))
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Opaleye (DefaultFromField, Field, sqlValueJSON, SqlJson, sqlStrictText, SqlText,
                sqlUUID, SqlUuid, ToFields, toToFields)
import Web.Internal.HttpApiData (FromHttpApiData)

newtype ProjectId = ProjectId UUID
  deriving stock (Generic, Show)
  deriving newtype (DefaultFromField SqlUuid, Eq, FromHttpApiData, FromJSON, Ord, ToJSON)

instance Default ToFields ProjectId (Field SqlUuid) where
  def = toToFields (sqlUUID . coerce)

newtype ProjectName = ProjectName Text
  deriving stock (Generic, Show)
  deriving newtype (DefaultFromField SqlText, Eq, FromJSON, Ord, ToJSON)

instance Default ToFields ProjectName (Field SqlText) where
  def = toToFields (sqlStrictText . coerce)

newtype ProjectDescription = ProjectDescription Text
  deriving stock (Generic, Show)
  deriving newtype (DefaultFromField SqlText, Eq, FromJSON, Ord, ToJSON)

instance Default ToFields ProjectDescription (Field SqlText) where
  def = toToFields (sqlStrictText . coerce)

newtype EntityTypeId = EntityTypeId UUID
  deriving stock (Generic, Show)
  deriving newtype (DefaultFromField SqlUuid, Eq, FromHttpApiData, FromJSON, Ord, ToJSON)

instance Default ToFields EntityTypeId (Field SqlUuid) where
  def = toToFields (sqlUUID . coerce)

newtype EntityTypeName = EntityTypeName Text
  deriving stock (Generic, Show)
  deriving newtype (DefaultFromField SqlText, Eq, FromJSON, Ord, ToJSON)

instance Default ToFields EntityTypeName (Field SqlText) where
  def = toToFields (sqlStrictText . coerce)

newtype EntityTypeDescriptor = EntityTypeDescriptor Value
  deriving stock (Generic, Show)
  deriving newtype (DefaultFromField SqlJson, Eq, FromJSON, Ord, ToJSON)

instance Default ToFields EntityTypeDescriptor (Field SqlJson) where
  def = toToFields (sqlValueJSON @Value . coerce)

newtype EntityId = EntityId UUID
  deriving stock (Generic, Show)
  deriving newtype (DefaultFromField SqlUuid, Eq, FromHttpApiData, FromJSON, Ord, ToJSON)

instance Default ToFields EntityId (Field SqlUuid) where
  def = toToFields (sqlUUID . coerce)

newtype AttributeId = AttributeId UUID
  deriving stock (Generic, Show)
  deriving newtype (DefaultFromField SqlUuid, Eq, FromHttpApiData, FromJSON, Ord, ToJSON)

instance Default ToFields AttributeId (Field SqlUuid) where
  def = toToFields (sqlUUID . coerce)

newtype AttributeName = AttributeName Text
  deriving stock (Generic, Show)
  deriving newtype (DefaultFromField SqlText, Eq, FromJSON, Ord, ToJSON, ToJSONKey)

instance Default ToFields AttributeName (Field SqlText) where
  def = toToFields (sqlStrictText . coerce)

newtype AttributeValue = AttributeValue Value
  deriving stock (Generic, Show)
  deriving newtype (DefaultFromField SqlJson, Eq, FromJSON, Ord, ToJSON)

instance Default ToFields AttributeValue (Field SqlJson) where
  def = toToFields (sqlValueJSON @Value . coerce)

newtype RelationshipId = RelationshipId UUID
  deriving stock (Generic, Show)
  deriving newtype (DefaultFromField SqlUuid, Eq, FromHttpApiData, FromJSON, Ord, ToJSON)

instance Default ToFields RelationshipId (Field SqlUuid) where
  def = toToFields (sqlUUID . coerce)

newtype RelationshipType = RelationshipType Text
  deriving stock (Generic, Show)
  deriving newtype (DefaultFromField SqlText, Eq, FromJSON, Ord, ToJSON)

instance Default ToFields RelationshipType (Field SqlText) where
  def = toToFields (sqlStrictText . coerce)


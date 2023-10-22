module Krinj.EntityService.Internal.Model.Newtypes where

import Data.Aeson (FromJSON, ToJSON, ToJSONKey, Value)
import Data.Coerce (coerce)
import Data.Profunctor.Product.Default (Default(..))
import Data.String (IsString(..))
import Data.Text (pack, Text, unpack)
import Data.UUID (toString, UUID)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)
import Opaleye (DefaultFromField, Field, sqlValueJSON, SqlJson, sqlStrictText, SqlText,
                sqlUUID, SqlUuid, ToFields, toToFields)
import Web.Internal.HttpApiData (FromHttpApiData)

newtype ProjectId = ProjectId UUID
  deriving stock (Generic)
  deriving newtype (DefaultFromField SqlUuid, Eq, FromHttpApiData, FromField, FromJSON, Ord, ToField, ToJSON)

instance Show ProjectId where
  show (ProjectId x) = toString x

instance Default ToFields ProjectId (Field SqlUuid) where
  def = toToFields (sqlUUID . coerce)

newtype ProjectName = ProjectName Text
  deriving stock (Generic)
  deriving newtype (DefaultFromField SqlText, Eq, FromJSON, Ord, ToJSON)

instance Show ProjectName where
  show (ProjectName t) = unpack t

instance IsString ProjectName where
  fromString = ProjectName . pack

instance Default ToFields ProjectName (Field SqlText) where
  def = toToFields (sqlStrictText . coerce)

newtype ProjectDescription = ProjectDescription Text
  deriving stock (Generic)
  deriving newtype (DefaultFromField SqlText, Eq, FromJSON, Ord, ToJSON)

instance Show ProjectDescription where
  show (ProjectDescription x) = unpack x

instance IsString ProjectDescription where
  fromString = ProjectDescription . pack

instance Default ToFields ProjectDescription (Field SqlText) where
  def = toToFields (sqlStrictText . coerce)

newtype EntityTypeId = EntityTypeId UUID
  deriving stock (Generic)
  deriving newtype (DefaultFromField SqlUuid, Eq, FromHttpApiData, FromField, FromJSON, Ord, ToField, ToJSON)

instance Show EntityTypeId where
  show (EntityTypeId x) = toString x

instance Default ToFields EntityTypeId (Field SqlUuid) where
  def = toToFields (sqlUUID . coerce)

newtype EntityTypeName = EntityTypeName Text
  deriving stock (Generic)
  deriving newtype (DefaultFromField SqlText, Eq, FromJSON, Ord, ToJSON)

instance Show EntityTypeName where
  show (EntityTypeName x) = unpack x

instance IsString EntityTypeName where
  fromString = EntityTypeName . pack

instance Default ToFields EntityTypeName (Field SqlText) where
  def = toToFields (sqlStrictText . coerce)

newtype EntityTypeDescriptor = EntityTypeDescriptor Value
  deriving stock (Generic)
  deriving newtype (DefaultFromField SqlJson, Eq, FromJSON, IsString, Ord, Show, ToJSON)

instance Default ToFields EntityTypeDescriptor (Field SqlJson) where
  def = toToFields (sqlValueJSON @Value . coerce)

newtype EntityId = EntityId UUID
  deriving stock (Generic)
  deriving newtype (DefaultFromField SqlUuid, Eq, FromHttpApiData, FromJSON, Ord, ToField, ToJSON)

instance Show EntityId where
  show (EntityId x) = toString x

instance Default ToFields EntityId (Field SqlUuid) where
  def = toToFields (sqlUUID . coerce)

newtype AttributeName = AttributeName Text
  deriving stock (Generic)
  deriving newtype (DefaultFromField SqlText, Eq, FromHttpApiData, FromJSON, Ord, ToJSON, ToJSONKey)

instance Show AttributeName where
  show (AttributeName x) = unpack x

instance IsString AttributeName where
  fromString = AttributeName . pack

instance Default ToFields AttributeName (Field SqlText) where
  def = toToFields (sqlStrictText . coerce)

newtype AttributeValue = AttributeValue Value
  deriving stock (Generic)
  deriving newtype (DefaultFromField SqlJson, Eq, FromJSON, Ord, Show, ToJSON)

instance Default ToFields AttributeValue (Field SqlJson) where
  def = toToFields (sqlValueJSON @Value . coerce)

newtype RelationshipId = RelationshipId UUID
  deriving stock (Generic)
  deriving newtype (DefaultFromField SqlUuid, Eq, FromHttpApiData, FromJSON, Ord, ToJSON)

instance Show RelationshipId where
  show (RelationshipId x) = toString x

instance Default ToFields RelationshipId (Field SqlUuid) where
  def = toToFields (sqlUUID . coerce)

newtype RelationshipType = RelationshipType Text
  deriving stock (Generic)
  deriving newtype (DefaultFromField SqlText, Eq, FromJSON, Ord, ToJSON)

instance Show RelationshipType where
  show (RelationshipType x) = unpack x

instance IsString RelationshipType where
  fromString = RelationshipType . pack

instance Default ToFields RelationshipType (Field SqlText) where
  def = toToFields (sqlStrictText . coerce)


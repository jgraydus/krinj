{-# LANGUAGE DerivingVia, DeriveAnyClass, TemplateHaskell #-}
module EntityService.Internal.Model.Attributes where

import Data.Aeson (FromJSON, ToJSON)
import Data.Profunctor.Product.TH
import Data.Time.Clock (UTCTime)
import EntityService.Internal.Model.Newtypes
import GHC.Generics (Generic)
import Opaleye

data AttributesRowT a b c d e = AttributesRowT
  { entityId :: a
  , name :: b
  , value :: c
  , createdAt :: d
  , modifiedAt :: e
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

$(makeAdaptorAndInstance' ''AttributesRowT)

type AttributesRowW =
  AttributesRowT (Field SqlUuid) (Field SqlText) (Field SqlJson)
                 (Maybe (Field SqlTimestamptz)) (Maybe (FieldNullable SqlTimestamptz))
type AttributesRowR =
  AttributesRowT (Field SqlUuid) (Field SqlText) (Field SqlJson)
                 (Field SqlTimestamptz) (FieldNullable SqlTimestamptz)

attributesTable :: Table AttributesRowW AttributesRowR
attributesTable = table "attributes" $
  pAttributesRowT AttributesRowT
  { entityId    = tableField "entity_id"
  , name        = tableField "name"
  , value       = tableField "value"
  , createdAt   = optionalTableField "created_at"
  , modifiedAt  = optionalTableField "modified_at"
  }

type Attribute = AttributesRowT EntityId AttributeName AttributeValue UTCTime (Maybe UTCTime)


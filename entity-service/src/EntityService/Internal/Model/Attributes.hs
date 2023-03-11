{-# LANGUAGE DerivingVia, DeriveAnyClass, TemplateHaskell #-}
module EntityService.Internal.Model.Attributes where

import Data.Aeson (FromJSON, ToJSON)
import Data.Profunctor.Product.TH
import EntityService.Internal.Model.Newtypes
import GHC.Generics (Generic)
import Opaleye

data AttributesRowT a b c = AttributesRowT
  { entityId :: a
  , name :: b
  , value :: c
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

$(makeAdaptorAndInstance' ''AttributesRowT)

type AttributesRowW = AttributesRowT (Field SqlUuid) (Field SqlText) (Field SqlJson)
type AttributesRowR = AttributesRowT (Field SqlUuid) (Field SqlText) (Field SqlJson)

attributesTable :: Table AttributesRowW AttributesRowR
attributesTable = table "attributes" $
  pAttributesRowT AttributesRowT
  { entityId    = tableField "entity_id"
  , name        = tableField "name"
  , value       = tableField "value"
  }

type Attribute = AttributesRowT EntityId AttributeName AttributeValue


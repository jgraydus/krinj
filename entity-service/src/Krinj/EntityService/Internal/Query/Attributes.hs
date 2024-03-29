module Krinj.EntityService.Internal.Query.Attributes (
    getAttribute, getAttributes
) where

import Data.Map.Strict (Map)
import Database.PostgreSQL.Simple (Connection)
import GHC.Records (getField)
import Krinj.EntityService.Internal
import Krinj.EntityService.Internal.Model
import Krinj.EntityService.Internal.Util
import Opaleye hiding (groupBy)

-- |
getAttribute :: Connection -> EntityId -> AttributeName -> IO (Result Attribute)
getAttribute conn entityId attributeName = do
  result <- runSelect conn select
  pure $ case result of
    attribute : _ -> Right attribute
    _             -> Left NotFound
  where
    select = do
      row@(AttributesRowT entityId' attributeName' _ _ _) <- selectTable attributesTable
      where_ $ entityId' .== toFields entityId .&& attributeName' .== toFields attributeName
      pure row

-- |
getAttributes :: Connection -> [EntityId] -> IO (Result (Map EntityId [Attribute]))
getAttributes conn entityIds = do
  attributes :: [Attribute] <- runSelect conn select
  pure . Right $ groupBy (getField @"entityId") attributes
  where
    select = do
      row@(AttributesRowT entityId1 _ _ _ _) <- selectTable attributesTable
      where_ $ in_ (fmap toFields entityIds) entityId1
      pure row


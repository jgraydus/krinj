module Krinj.EntityService.Internal where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Page = Page
  { pageNumber :: Int
  , resultsPerPage :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Error = NotFound | InsertFailed | UpdateFailed | DeleteFailed | Other
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Result a = Either Error a


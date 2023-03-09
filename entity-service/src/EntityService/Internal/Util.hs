module EntityService.Internal.Util (
    groupBy, indexBy
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

groupBy :: Ord k => (v -> k) -> [v] -> Map k [v]
groupBy f = Map.unionsWith (<>) . fmap g
  where g v = Map.singleton (f v) [v]

indexBy :: Ord k => (v -> k) -> [v] -> Map k v
indexBy f = Map.fromList . map g
  where g x = (f x, x)


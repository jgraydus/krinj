module EntityService.Internal.Util (
    groupBy, indexBy
) where

import Data.Map (Map)
import Data.Map qualified as Map

groupBy :: Ord k => [v] -> (v -> k) -> Map k [v]
groupBy vs f = (Map.unionsWith (<>) . fmap g) vs
  where g v = Map.singleton (f v) [v]

indexBy :: Ord k => [v] -> (v -> k) -> Map k v
indexBy vs f = (Map.fromList . map g) vs
  where g x = (f x, x)


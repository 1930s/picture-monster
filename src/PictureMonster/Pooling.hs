-- | Module exposing the functionality of image download pooling.
-- To respect the per-host download limits imposed by the user, the 'URI's to download are split into pools,
-- which are maximal sets of images that can be downloaded concurrently without violating the limits.
module PictureMonster.Pooling (pool) where

import qualified Data.Map.Strict as M
import Data.Maybe                       (maybe)
import Network.URI
import PictureMonster.Data

-- | Splits the 'URI's supplied into pools, adhering to the connection limits set by the user.
pool :: ConnectionLimits    -- ^ Contains the connection limits set by the user.
     -> [URI]               -- ^ List of 'URI's to split into pools.
     -> [URIPool]           -- ^ List of resulting 'URIPool's.
pool (Limits _ perHost) uris = constructPools perHost $ splitByAuthority uris

-- | Transforms the 'URI's supplied into pairs, containing the authority name and the full 'URI'.
-- Used to split the 'URI's into pools.
extractAuthority :: [URI]               -- ^ List of 'URI's to transform.
                 -> [(String, [URI])]   -- ^ List of authority name-'URI' pairs.
extractAuthority = map extract'
    where   extract' :: URI -> (String, [URI])
            extract' uri = (maybe "" uriRegName (uriAuthority uri), [uri])

-- | Puts the 'URI's from the list supplied into a map, whose keys are the authority names,
-- and the values are lists of 'URI's originating from that authority.
-- This allows for an easier implementation of the split operation.
splitByAuthority :: [URI]               -- ^ List of 'URI's to group by authority.
                 -> M.Map String [URI]  -- ^ Resulting 'M.Map', grouping the 'URI's by authority.
splitByAuthority uris = M.fromListWith (++) $ extractAuthority uris

-- | Performs the pooling operation, adhering to the limits imposed.
constructPools :: ConnectionLimit   -- ^ 'ConnectionLimit' imposed on the pools.
               -> M.Map a [b]       -- ^ Instance of 'M.Map' containing the grouping.
               -> [[b]]             -- ^ The constructed pools, as a list of lists.
constructPools limit map
    | M.null map    = []
    | otherwise     = constructPool limit map:constructPools limit (removeUsed limit map)

-- | Takes the maximum allowed number of items from each group in the map and constructs a pool from them.
constructPool :: ConnectionLimit    -- ^ 'ConnectionLimit' imposed on the pools.
              -> M.Map a [b]        -- ^ Instance of 'M.Map' containing the grouping.
              -> [b]                -- ^ Constructed item pool.
constructPool (Limit l) = M.foldr ((++) . take l) []
constructPool NoLimit = M.foldr (++) []

-- | Removes the items used in a constructed pool from a map.
removeUsed :: ConnectionLimit   -- ^ 'ConnectionLimit' imposed on the pools.
           -> M.Map a [b]       -- ^ Instance of 'M.Map' containing the grouping.
           -> M.Map a [b]       -- ^ Instance of 'M.Map' containing the grouping, with the items used removed.
removeUsed (Limit l) map = M.filter (not . null) $ M.map (drop l) map
removeUsed NoLimit _ = M.empty

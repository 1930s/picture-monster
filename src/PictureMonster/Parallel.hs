-- | Module used to perform parallel operations during the crawling process
-- using the limits imposed by the user.
module PictureMonster.Parallel where

import qualified Control.Concurrent.ParallelIO.Global as G
import qualified Control.Concurrent.ParallelIO.Local as L
import PictureMonster.Data

-- | Analogue to 'mapM' for 'IO', executing each 'IO' action in parallel.
mapParallel :: ConnectionLimit  -- ^ The connection limit imposed by the user.
            -> (a -> IO b)      -- ^ Function transforming a list element into an 'IO' action.
            -> [a]              -- ^ List to transform.
            -> IO [b]           -- ^ List of collected results of the 'IO' operations, wrapped in an 'IO' monad.
mapParallel limit f = parallel limit . map f

-- | Analogue to the global 'G.parallel' and local 'L.parallel', alternating between them to adhere to parallelism
-- limits imposed by the user.
parallel :: ConnectionLimit -- ^ The connection limit imposed by the user.
         -> [IO a]          -- ^ List of 'IO' actions to perform.
         -> IO [a]          -- ^ Collected results of the 'IO' actions.
parallel (Limit l) ops = L.withPool l $ \pool -> L.parallel pool ops
parallel NoLimit ops = G.parallel ops

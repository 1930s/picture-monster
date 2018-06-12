-- | Module containing data structures used by the application.
module PictureMonster.Data where

import qualified Data.Set as S
import Network.URI

-- | Identification number assigned to a previously started crawling session that has been interrupted.
-- Used to continue the session at a later data.
type SessionId = Int

-- | Type used to store the maximum search depth during crawling.
type SearchDepth = Int

-- | Type used to represent image extensions.
type Extension = String

-- | Type used to represent a pool of 'URI's that can be downloaded concurrently.
type URIPool = [URI]

-- | Represents a limit of HTTP connections that can be used during crawling.
data ConnectionLimit
    -- | Limited number of allowed HTTP connections.
    = Limit Int
    -- | Unlimited number of allowed HTTP connections.
    | NoLimit deriving Show

-- | Collects the connection limits imposed by the user.
data ConnectionLimits = Limits {
    -- | Limit of all outgoing HTTP connections.
    total :: ConnectionLimit,
    -- | Limit of outgoing HTTP connections to a single host.
    perHost :: ConnectionLimit
} deriving Show

-- | Contains the data concerning a single crawling session.
data SessionData = SessionData {
    -- | List of initial 'URI's to begin crawling from.
    uris :: [URI],
    -- | Maximum search depth.
    depth :: SearchDepth,
    -- | Extension of images to be searched.
    extension :: Maybe String,
    -- | Path to parent directory where the file should be saved.
    targetDir :: FilePath
} deriving Show

-- | Represents the command chosen by the user.
data Command
    -- | Starts a new crawling session.
    -- The crawler will begin from a supplied list of URIs.
    -- The crawling and image downloading process will adhere to the specified connection limits.
    = NewSession SessionData ConnectionLimits FilePath
    -- | Continues a previously interrupted crawling session with the supplied ID.
    -- The crawling and image downloading process will adhere to the specified connection limits.
    | ExistingSession FilePath ConnectionLimits

-- | Data structure holding the current crawling state.
data CrawlState = State {
    links :: S.Set URI,
    images :: S.Set URI
} deriving Show

-- | Merges the two crawl states.
-- The list of link URLs in the first state is ignored.
-- The sets of image URLs are merged.
stateUnion :: CrawlState        -- ^ First crawl state.
           -> CrawlState        -- ^ Second crawl state.
           -> CrawlState        -- ^ Resulting crawl state.
stateUnion first second = State (links second) $ S.union (images first) (images second)

data CrawlLayer = Layer {
    remainingDepth :: SearchDepth,
    state :: CrawlState
} deriving Show

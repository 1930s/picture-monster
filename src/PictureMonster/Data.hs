-- | Module containing data structures used by the application.
module PictureMonster.Data where

import Network.URI

-- | Identification number assigned to a previously started crawling session that has been interrupted.
-- Used to continue the session at a later data.
type SessionId = Int

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

-- | Represents the command chosen by the user.
data Command
    -- | Starts a new crawling session.
    -- The crawler will begin from a supplied list of URIs.
    -- The crawling and image downloading process will adhere to the specified connection limits.
    = NewSession [URI] ConnectionLimits
    -- | Continues a previously interrupted crawling session with the supplied ID.
    -- The crawling and image downloading process will adhere to the specified connection limits.
    | ExistingSession SessionId ConnectionLimits
    -- | Lists all interrupted crawling sessions with their IDs.
    | ListSessions deriving Show

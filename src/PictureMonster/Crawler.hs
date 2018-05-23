{-# LANGUAGE OverloadedStrings #-}
-- | Module responsible for performing web crawling, constructing the page reference tree and collecting image URLs.
module PictureMonster.Crawler (crawl) where

import Control.Monad
import qualified Data.Text as T
import Data.Maybe               (mapMaybe)
import Data.Set                 (Set)
import qualified Data.Set as S
import Network.HTTP.Simple
import Network.URI
import PictureMonster.Data
import Text.HTML.DOM
import Text.XML                 (Document)
import Text.XML.Cursor

-- | Responsible for performing the crawling operation.
crawl :: SessionData        -- ^ Structure containing the initial session data.
      -> ConnectionLimits   -- ^ Structure describing the connection limits specified by the user. (TODO)
      -> IO ()
crawl (SessionData uris depth) limits = crawlRecursion depth uris >>= print

-- | Recursive function responsible for constructing and crawling the page tree.
crawlRecursion :: SearchDepth   -- ^ Remaining search depth.
               -> [URI]         -- ^ Current list of URIs found.
               -> IO (Set URI)  -- ^ Resulting 'Set' of URIs found, wrapped in an 'IO' monad.
crawlRecursion 0 uris = return $ S.fromList uris
crawlRecursion n uris = S.union (s.fromList urls) <$> (concat <$> mapM getLinks uris) >>= (crawlRecursion $! (n - 1))

-- | Fetches the contents of the webpage with the supplied URI and returns a list of links contained in the page.
getLinks :: URI         -- ^ URI of the webpage to crawl.
         -> IO [URI]    -- ^ Resulting absolute links found in the page, wrapped in an 'IO' monad.
getLinks uri = filterLinks . resolveLinks uri . parseLinks . findLinks <$> (createRequest uri >>= getDocument)

-- | Creates a HTTP 'Request' object from an 'URI'.
createRequest :: URI -> IO Request
createRequest uri = parseRequest $ uriToString id uri ""

-- | Fetches the contents of a 'Document', using the supplied 'Request'.
getDocument :: Request -> IO Document
getDocument req = httpSink req $ const sinkDoc

-- | Searches for @<a>@ elements in the supplied HTML document and returns the values of their @href@ attributes.
findLinks :: Document -> [String]
findLinks doc = map T.unpack $ concat $ cursor $// element "a" &| attribute "href"
    where   cursor = fromDocument doc

-- | Parses a list of strings into 'URI's.
-- Strings that do not constitute valid 'URI's are discarded.
parseLinks :: [String] -> [URI]
parseLinks = mapMaybe parseURIReference

-- | Converts potentially relative 'URI's into absolute ones.
resolveLinks :: URI     -- ^ Base 'URI' to use during resolution.
             -> [URI]   -- ^ List of 'URI's to resolve.
             -> [URI]   -- ^ Resulting list of resolved 'URI's.
resolveLinks base = map $ discardQueryAndFragment . (`relativeTo` base)

-- | Scrubs an 'URI' instance, clearing the query string and fragment identifier.
-- Used to remove 'URI's that point to the same document, but differ in query and fragment.
discardQueryAndFragment :: URI -> URI
discardQueryAndFragment (URI scheme authority path query fragment) = URI scheme authority path "" ""

-- | Filters the 'URI's in the list supplied, leaving only the ones using the @http:@ or @https:@ schemes.
filterLinks :: [URI]    -- ^ List of 'URI's to filter.
            -> [URI]    -- ^ Filtered list of 'URI's with the proper schemes.
filterLinks = filter isHttp
    where   isHttp :: URI -> Bool
            isHttp uri = uriScheme uri == "http:" || uriScheme uri == "https:"

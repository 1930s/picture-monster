{-# LANGUAGE OverloadedStrings #-}
-- | Module responsible for performing web crawling, constructing the page reference tree and collecting image URLs.
module PictureMonster.Crawler (crawl) where

import Control.Monad
import qualified Data.Text as T
import Data.Maybe               (mapMaybe)
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
crawl (SessionData uris depth) limits = (length <$> crawlRecursion depth uris) >>= print

-- | Recursive function responsible for constructing and crawling the page tree.
crawlRecursion :: SearchDepth   -- ^ Remaining search depth.
               -> [URI]         -- ^ Current list of URIs found.
               -> IO [URI]      -- ^ Resulting list of URIs found, wrapped in an 'IO' monad.
crawlRecursion 0 uris = return uris
crawlRecursion n uris = (concat <$> mapM getLinks uris) >>= (crawlRecursion $! (n - 1)) >>= appendChildren
    where   appendChildren :: [URI] -> IO [URI]
            appendChildren children = return $ uris ++ children

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
resolveLinks base = map (`relativeTo` base)

-- | Filters the 'URI's in the list supplied, leaving only the ones using the @http:@ or @https:@ schemes.
filterLinks :: [URI]    -- ^ List of 'URI's to filter.
            -> [URI]    -- ^ Filtered list of 'URI's with the proper schemes.
filterLinks = filter isHttp
    where   isHttp :: URI -> Bool
            isHttp uri = uriScheme uri == "http:" || uriScheme uri == "https:"

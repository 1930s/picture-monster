{-# LANGUAGE OverloadedStrings #-}
-- | Module responsible for performing web crawling, constructing the page reference tree and collecting image URLs.
module PictureMonster.Crawler (crawl) where

import Control.Monad
import qualified Data.Text as T
import Data.Maybe               (mapMaybe)
import Data.List                (isSuffixOf)
import Data.Set                 (Set)
import qualified Data.Set as S
import Network.HTTP.Simple
import Network.URI
import PictureMonster.Data
import PictureMonster.Parallel
import PictureMonster.Network
import Text.HTML.DOM
import Text.XML                 (Document)
import Text.XML.Cursor

-- | Data structure holding the current crawling state.
-- TODO: potentially change to StateT
data CrawlState = State {
    links :: [URI],
    images :: Set URI
} deriving Show

-- | Responsible for performing the crawling operation.
crawl :: SessionData        -- ^ Structure containing the initial session data.
      -> ConnectionLimits   -- ^ Structure describing the connection limits specified by the user.
      -> IO [URI]           -- ^ List of images found.
crawl (SessionData uris depth ext _) (Limits total _) = S.toList <$> (filterExt ext <$> crawlRecursion total depth (State uris $ S.fromList []))

-- | Filters the set of images found during crawling.
filterExt :: Maybe Extension    -- ^ Extension of images to use for filtering.
          -> CrawlState         -- ^ The final crawling state.
          -> Set URI            -- ^ The filtered set of 'URI's to download.
filterExt Nothing state = images state
filterExt (Just ext) (State _ imgs) = S.filter (testExt ext) imgs
    where   testExt :: Extension -> URI -> Bool
            testExt ext uri = ext `isSuffixOf` uriPath uri

-- | Recursive function responsible for constructing and crawling the page tree.
crawlRecursion :: ConnectionLimit
               -> SearchDepth   -- ^ Remaining search depth.
               -> CrawlState    -- ^ Current list of URIs found.
               -> IO CrawlState -- ^ Resulting 'Set' of URIs found, wrapped in an 'IO' monad.
crawlRecursion limit 0 uris = return uris
crawlRecursion limit n uris = stateUnion uris <$> ((concatState <$> mapParallel limit getContent (links uris)) >>= (crawlRecursion limit $! (n - 1)))

-- | Concatenates two 'CrawlState' instances.
concatState :: [CrawlState]         -- ^ List of 'CrawlState' instances being concatenated.
            -> CrawlState           -- ^ Resulting concatenated 'CrawlState'.
concatState [] = State [] $ S.fromList []
concatState (x:xs) = State (links x ++ links ys) $ S.union (images x) (images ys)
    where   ys = concatState xs

-- | Merges the two crawl states.
-- The list of link URLs in the first state is ignored.
-- The sets of image URLs are merged.
stateUnion :: CrawlState        -- ^ First crawl state.
           -> CrawlState        -- ^ Second crawl state.
           -> CrawlState        -- ^ Resulting crawl state.
stateUnion first second = State (links second) $ S.union (images first) (images second)

-- | Gets all links and image URIs found on a webpage.
getContent :: URI           -- ^ URI of the webpage to crawl.
           -> IO CrawlState -- ^ Result of the crawl, containing the links and images found.
getContent uri = liftM2 State (getLinks uri document) (getImages uri document)
    where   document = createRequest uri >>= getDocument

-- | Returns a list of links contained in the page.
getLinks :: URI         -- ^ URI of the webpage being crawled.
         -> IO Document -- ^ The document to search, wrapped in an 'IO' monad.
         -> IO [URI]    -- ^ Resulting absolute links found in the page, wrapped in an 'IO' monad.
getLinks uri document = processLinks uri . findLinks <$> document -- (createRequest uri >>= getDocument)

-- | Returns a list of images contained in the page.
getImages :: URI            -- ^ URI of the webpage being crawled.
          -> IO Document    -- ^ The document to search, wrapped in an 'IO' monad.
          -> IO (Set URI)   -- ^ Resulting image links found in the page. wrapped in an 'IO' monad.
getImages uri document = S.fromList . processLinks uri . findImages <$> document

-- | Fetches the contents of a 'Document', using the supplied 'Request'.
getDocument :: Request -> IO Document
getDocument req = httpSink req $ const sinkDoc

-- | Searches for @<a>@ elements in the supplied HTML document and returns the values of their @href@ attributes.
findLinks :: Document -> [String]
findLinks doc = map T.unpack $ concat $ cursor $// element "a" &| attribute "href"
    where   cursor = fromDocument doc

-- | Searches for @<img>@ elements in the supplied HTML document and returns the values of their @src@ attributes.
findImages :: Document -> [String]
findImages doc = map T.unpack $ concat $ cursor $// element "img" &| attribute "src"
    where   cursor = fromDocument doc

-- | Performs processing of the links supplied.
-- Eliminates invalid links, strips the query and fragment parts, and keeps only the 'URI's using the @http:@ or @https:@ schemes.
processLinks :: URI -> [String] -> [URI]
processLinks uri = filterLinks . resolveLinks uri . parseLinks

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

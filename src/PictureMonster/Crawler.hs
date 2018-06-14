{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | Module responsible for performing web crawling, constructing the page reference tree and collecting image URLs.
module PictureMonster.Crawler (crawl, continueCrawl) where

import Control.Exception        (try)
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
import PictureMonster.Serializer
import System.IO
import Text.HTML.DOM
import Text.XML                 (Document)
import Text.XML.Cursor

-- | Responsible for performing the crawling operation.
crawl :: Handle             -- ^ Handle to the file that contains the crawling session state.
      -> SessionData        -- ^ Structure containing the initial session data.
      -> ConnectionLimits   -- ^ Structure describing the connection limits specified by the user.
      -> IO [URI]           -- ^ List of images found.
crawl handle session (Limits total _) = crawlingHeader handle >>
    getUriList handle session total (initialLayer session) >>=
    \uris -> endSession handle >> return uris

-- | Creates an empty initial layer based on the starting session data.
initialLayer :: SessionData -- ^ Structure containing the initial session data.
             -> CrawlLayer  -- ^ Initial crawling layer.
initialLayer (SessionData uris depth _ _) = Layer depth (State (S.fromList uris) S.empty)

-- | Continues a crawling session from a layer.
continueCrawl :: SessionData        -- ^ Structure containing the initial session data.
              -> ConnectionLimits   -- ^ Connection limits imposed on the session.
              -> CrawlLayer         -- ^ Layer to continue the crawling from.
              -> IO [URI]           -- ^ List of 'URI's to download, wrapped in an 'IO' monad.
continueCrawl session (Limits total _) = getUriList' session total

-- | Fetches a list of 'URI's to download, creating a crawling report in the process.
getUriList :: Handle                -- ^ Handle used to write the crawling report to.
           -> SessionData           -- ^ Structure containing the initial session data.
           -> ConnectionLimit       -- ^ Limit on total connections imposed by the user.
           -> CrawlLayer            -- ^ Layer to start the crawling from.
           -> IO [URI]              -- ^ List of 'URI's to download, wrapped in an 'IO' monad.
getUriList handle (SessionData _ _ ext _) total (Layer depth state) =
    S.toList . images <$> crawlRecursion handle ext total depth state

-- | Fetches a list of 'URI's to download without creating a report.
getUriList' :: SessionData          -- ^ Structure containing the initial session data.
            -> ConnectionLimit      -- ^ Limit on total connections imposed by the user.
            -> CrawlLayer           -- ^ Layer to start the crawling from.
            -> IO [URI]             -- ^ List of 'URI's to download, wrapped in an 'IO' monad.
getUriList' (SessionData _ _ ext _) total (Layer depth state) =
    S.toList . images <$> crawlRecursion' ext total (depth - 1) state

-- | Recursive function responsible for constructing and crawling the page tree.
-- Creates a crawling report in the process.
crawlRecursion :: Handle            -- ^ Handle to the file that contains the crawling session state.
               -> Maybe Extension   -- ^ Extension of images to be downloaded.
               -> ConnectionLimit   -- ^ Total connection limit imposed by the user.
               -> SearchDepth       -- ^ Remaining search depth.
               -> CrawlState        -- ^ Current crawling state.
               -> IO CrawlState     -- ^ Resulting crawling state, wrapped in an 'IO' monad.
crawlRecursion handle ext limit n uris
    | n <= 0    = return uris
    | otherwise = (stateUnion uris <$>
                        (putLayer handle n >>
                        concatState <$> mapParallel limit (getContent ext) (S.toList $ links uris) >>=
                        (\state -> putLayerState handle state >> return state))) >>=
                        (crawlRecursion handle ext limit $! (n - 1))

-- | Recursive function responsible for constructing and crawling the page tree.
-- Does not create a crawling report in the process.
crawlRecursion' :: Maybe Extension  -- ^ Extension of images to be downloaded.
                -> ConnectionLimit  -- ^ Total connection limit imposed by the user.
                -> SearchDepth      -- ^ Remaining search depth.
                -> CrawlState       -- ^ Current crawling state.
                -> IO CrawlState    -- ^ Resulting crawling state, wrapped in an 'IO' monad.
crawlRecursion' ext limit n uris
    | n <= 0    = return uris
    | otherwise = (stateUnion uris <$>
                        (concatState <$> mapParallel limit (getContent ext) (S.toList $ links uris))) >>=
                        (crawlRecursion' ext limit $! (n - 1))

-- | Concatenates two 'CrawlState' instances.
concatState :: [CrawlState]         -- ^ List of 'CrawlState' instances being concatenated.
            -> CrawlState           -- ^ Resulting concatenated 'CrawlState'.
concatState [] = State S.empty S.empty
concatState (x:xs) = State (S.union (links x) (links ys)) $ S.union (images x) (images ys)
    where   ys = concatState xs

-- | Gets all links and image URIs found on a webpage.
getContent :: Maybe Extension
           -> URI           -- ^ URI of the webpage to crawl.
           -> IO CrawlState -- ^ Result of the crawl, containing the links and images found.
getContent ext uri = response >>= \case
    (Left  error)    -> putStr "Cannot fetch contents of page " >> print uri >> return (State S.empty S.empty)
    (Right document) -> liftM2 State (getLinks uri document) (getImages ext uri document)
    where   response = createRequest uri >>= getDocument

-- | Returns a list of links contained in the page.
getLinks :: URI             -- ^ URI of the webpage being crawled.
         -> Document     -- ^ The document to search, wrapped in an 'IO' monad.
         -> IO (Set URI)    -- ^ Resulting absolute links found in the page, wrapped in an 'IO' monad.
getLinks uri = return . S.fromList . processLinks uri . findLinks

-- | Returns a list of images contained in the page.
getImages :: Maybe Extension
          -> URI            -- ^ URI of the webpage being crawled.
          -> Document       -- ^ The document to search, wrapped in an 'IO' monad.
          -> IO (Set URI)   -- ^ Resulting image links found in the page. wrapped in an 'IO' monad.
getImages (Just ext) uri document = S.filter (checkExt ext) <$> getImages Nothing uri document
    where   checkExt :: Extension -> URI -> Bool
            checkExt ext uri = ext `isSuffixOf` uriPath uri
getImages Nothing uri document = (return . S.fromList . processLinks uri . findImages) document

-- | Fetches the contents of a 'Document', using the supplied 'Request'.
getDocument :: Request -> IO (Either HttpException Document)
getDocument req = try (httpSink req $ const sinkDoc)

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

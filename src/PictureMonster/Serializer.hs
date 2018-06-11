-- | Module used to serialize data passing through the application.
module PictureMonster.Serializer
    (serializeSession,
    crawlingHeader,
    putLayer,
    putLayerState,
    endSession)
where

import qualified Data.Set as S
import Network.URI
import PictureMonster.Data
import System.IO

-- | Serializes the data associated with a session to the supplied 'Handle'.
serializeSession :: Handle      -- 'Handle' of the file to write to.
                 -> SessionData -- 'SessionData' object to be serialized.
                 -> IO ()
serializeSession handle session = putHeader handle >>
    putUris handle (uris session) >>
    putDepth handle (depth session) >>
    putExtension handle (extension session) >>
    putTargetDir handle (targetDir session)

-- | Prints the header of the file.
putHeader :: Handle -> IO ()
putHeader handle = hPutStrLn handle "# Picture Monster" >>
    hPutStrLn handle "## Session information"

-- | Prints a list of 'URI's, formatted as a bullet list, to the handle.
putUris :: Handle -> [URI] -> IO ()
putUris handle uris = hPutStrLn handle "* Starting URLs:" >>
    mapM_ (\uri -> hPutStr handle "\t- " >> backquote handle (show uri)) uris

-- | Prints 'SearchDepth' information to the handle.
putDepth :: Handle -> SearchDepth -> IO ()
putDepth handle depth = hPutStr handle "* Maximum search depth: " >> hPutStrLn handle (show depth)

-- | Prints the desired file extension to the handle.
putExtension :: Handle -> Maybe String -> IO ()
putExtension handle extension = hPutStr handle "* Target file extension: " >>
    case extension of
        Nothing     -> hPutStrLn handle "None"
        Just ext    -> backquote handle ext

-- | Prints the target directory path to the handle.
putTargetDir :: Handle -> FilePath -> IO ()
putTargetDir handle path = hPutStr handle "* Target directory: " >> backquote handle path

-- | Prints the supplied string in backquotes to the handle.
backquote :: Handle -> String -> IO ()
backquote handle str = hPutStr handle "`" >> hPutStr handle str >> hPutStrLn handle "`"

-- | Prints the markdown header for the crawling report.
crawlingHeader :: Handle -> IO ()
crawlingHeader handle = hPutStrLn handle "## Crawling report"

-- | Prints the header for a crawling layer.
putLayer :: Handle -> SearchDepth -> IO ()
putLayer handle depth = hPutStr handle "### Layer " >> hPutStrLn handle (show depth)

-- | Saves the crawling state from a layer to the handle.
putLayerState :: Handle -> CrawlState -> IO ()
putLayerState handle state = hPutStrLn handle "#### Links found" >>
    putUriList handle (links state) >>
    hPutStrLn handle "#### Images found" >>
    putUriList handle (images state) >>
    hPutStrLn handle "__Layer complete.__"

-- | Prints a 'Foldable' of 'URI's as a markdown list to the handle.
putUriList :: (Foldable f) => Handle -> f URI -> IO ()
putUriList handle uris = mapM_ (putUri handle) uris

-- | Prints an 'URI' to the handle, as a markdown item.
putUri :: Handle -> URI -> IO ()
putUri handle uri = hPutStr handle "* " >> backquote handle (show uri)

endSession :: Handle -> IO ()
endSession handle = hPutStrLn handle "__Report complete.__"

{-# LANGUAGE OverloadedStrings #-}
-- | Module responsible for downloading and saving the images found to disk.
module PictureMonster.Downloader (download) where

import Conduit                  (withSinkFile)
import Control.Monad            (void)
import Network.HTTP.Simple      (Request, httpSink)
import Network.URI
import PictureMonster.Data
import PictureMonster.Parallel
import PictureMonster.Network
import System.Directory         (doesFileExist, createDirectoryIfMissing)
import System.FilePath          (takeDirectory, (</>))

-- | Downloads the files found during crawling, adhering to the imposed connection limits.
download :: SessionData         -- ^ Structure containing 'SessionData' for the current session.
         -> ConnectionLimits    -- ^ 'ConnectionLimits' that need to be preserved.
         -> [URIPool]           -- ^ List of 'URIPool's to download.
         -> IO ()
download session limits = mapM_ (downloadPool directory limits)
    where   directory = targetDir session

-- | Downloads all the files from a single 'URIPool' in parallel.
downloadPool :: FilePath            -- ^ Path to target download directory.
             -> ConnectionLimits    -- ^ 'ConnectionLimits' that need to be preserved.
             -> URIPool             -- ^ 'URIPool' to download.
             -> IO ()
downloadPool dir (Limits total _) pool = void $ mapParallel total (downloadURI dir) pool

-- | Constructs the target file path from the name of the file to download.
-- If the 'URI' of the image is, for instance
-- @https://www.example.com/test/test_img.png@
-- then the image should be saved in the directory
-- @./www.example.com/test/test_img.png@
constructPath :: FilePath   -- ^ Path to target download directory.
              -> URI        -- ^ 'URI' of the image for which to construct a path.
              -> FilePath   -- ^ The resulting 'FilePath' under which the image will be saved.
constructPath dir uri = dir </> maybe "" uriRegName (uriAuthority uri) ++ uriPath uri

-- | Downloads the file from the 'URI' supplied onto the disk.
downloadURI :: FilePath -- ^ Path to target download directory.
            -> URI      -- ^ 'URI' of the document to download.
            -> IO ()
downloadURI dir uri = ensureDirectory path >> downloadFile uri path
    where   path = constructPath dir uri

-- | Ensures that the directory for the 'FilePath' supplied exists.
ensureDirectory :: FilePath -- ^ 'FilePath' to check the parent directory of.
                -> IO ()
ensureDirectory path = createDirectoryIfMissing True $ takeDirectory path

-- | Fetches the contents of the document with the supplied 'URI' and saves it to a file with the path specified.
downloadFile :: URI         -- ^ 'URI' of the document to download.
             -> FilePath    -- ^ 'FilePath' to save the document to.
             -> IO ()
downloadFile uri path = doesFileExist path >>=
    \exists -> case exists of
        False -> createRequest uri >>= saveToFile path
        True  -> return ()

-- | Saves the results of the supplied 'Request' to the 'FilePath' provided.
saveToFile :: FilePath  -- ^ The 'FilePath' under which the file should be saved.
           -> Request   -- ^ The 'Request' whose result should be saved to the file.
           -> IO ()
saveToFile path req = withSinkFile path (httpSink req . const)

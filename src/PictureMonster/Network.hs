{-# LANGUAGE OverloadedStrings #-}
-- | Common module for networking manipulations.
module PictureMonster.Network where

import Network.HTTP.Simple
import Network.URI

-- | Creates a HTTP 'Request' object from an 'URI'.
createRequest :: URI -> IO Request
createRequest uri = parseRequest $ uriToString id uri ""

module PictureMonster.Parser (parseReport) where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import qualified Data.Set as S
import Network.URI
import PictureMonster.Data
import Text.Parsec hiding (State)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Read

-- | Type for parsers used.
type Parser = Parsec String ()

-- | Parses a single report loaded as a 'String'.
parseReport :: String                                               -- ^ Contents of the report to parse.
            -> Either ParseError (SessionData, Maybe CrawlLayer)    -- ^ An 'Either' monad, containing either the report contents, or an error message.
parseReport = runParser parseSession () ""

-- | Parses all data contained in a report.
parseSession :: Parser (SessionData, Maybe CrawlLayer)
parseSession = liftM2 (,) parseSessionInfo $ try (Just <$> parseLayers) <|> return Nothing

-- | A parser that takes a value, skips the next newline and returns the value supplied.
skipNewLine :: a -> Parser a
skipNewLine val = endOfLine >> return val

-- | Parses a single 'SessionData' instance.
-- Its contents are constructed from the first section of the report.
parseSessionInfo :: Parser SessionData
parseSessionInfo = string "# Picture Monster" >> endOfLine >>
    string "## Session information" >> endOfLine >>
    SessionData <$> parseStartingUrls <*>
    parseInitialDepth <*>
    parseFileExtension <*>
    parseTargetDirectory >>=
    skipNewLine

-- | Parses the list of starting 'URI's in the session to a list.
parseStartingUrls :: Parser [URI]
parseStartingUrls = string "* Starting URLs:" >> endOfLine >> endBy1 parseStartingUrl endOfLine

-- | Parses a single starting 'URI' in the session.
parseStartingUrl :: Parser URI
parseStartingUrl = tab >> string "- " >> parseBackquotedUri

-- | Parses an 'URI' contained within backquotes.
parseBackquotedUri :: Parser URI
parseBackquotedUri = parseBackquote >>= \s -> case parseURI s of
    Nothing     -> parserZero
    Just uri    -> return uri

-- | Parses a backquoted string.
parseBackquote :: Parser String
parseBackquote = between (char '`') (char '`') (many $ noneOf "`")

-- | Parses the initial 'SearchDepth' value.
parseInitialDepth :: Parser SearchDepth
parseInitialDepth = string "* Maximum search depth: " >> parseDepth

-- | Parses text into a 'SearchDepth' value.
parseDepth :: Parser SearchDepth
parseDepth = many1 digit >>= (\d -> case readMaybe d of
        Nothing     -> parserZero
        Just depth  -> return depth
    ) >>= skipNewLine

-- | Parses the file extension supplied by the user (or notes the lack thereof)
parseFileExtension :: Parser (Maybe Extension)
parseFileExtension = string "* Target file extension: " >>
    (Just <$> parseBackquote <|> (string "None" >> return Nothing)) >>= 
    skipNewLine

-- | Parses the target directory supplied by the user.
parseTargetDirectory :: Parser FilePath
parseTargetDirectory = string "* Target directory: " >> parseBackquote

-- | Parses the layers found in the file.
parseLayers :: Parser CrawlLayer
parseLayers = layerUnion <$> (string "## Crawling report" >> endOfLine >>
    some (try parseLayer))

-- | Performs a union operation on the list of supplied 'CrawlLayer's.
layerUnion :: [CrawlLayer] -> CrawlLayer
layerUnion [] = Layer 0 $ State S.empty S.empty
layerUnion [x] = x
layerUnion (x:xs) = Layer (remainingDepth u) $ stateUnion (state x) (state u)
    where u = layerUnion xs

-- | Parses a single 'CrawlLayer'.
parseLayer :: Parser CrawlLayer
parseLayer = Layer <$> parseLayerHeader <*> parseLayerContents >>=
    \layer -> parseEnd "Layer complete." >> return layer

-- | Parses a single layer's header.
parseLayerHeader :: Parser SearchDepth
parseLayerHeader = string "### Layer " >> parseDepth

-- | Parses a single layer's contents.
parseLayerContents :: Parser CrawlState
parseLayerContents = State <$> parseLayerSet "Links found" <*> parseLayerSet "Images found"

-- | Parses a section in the layer with the supplied title.
parseLayerSet :: String -> Parser (S.Set URI)
parseLayerSet header = string "#### " >> string header >> endOfLine >> parseUriList

-- | Parses a bullet list of backquoted 'URI's.
parseUriList :: Parser (S.Set URI)
parseUriList = S.fromList <$> many (string "* " >> parseBackquotedUri >>= skipNewLine)

-- | Indicates the end of a crawling layer.
parseEnd :: String -> Parser ()
parseEnd msg = string "__" >> string msg >> string "__" >> endOfLine >> return ()

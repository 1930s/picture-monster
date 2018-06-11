module PictureMonster.Parser where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import qualified Data.Set as S
import Network.URI
import PictureMonster.Data
import Text.Parsec hiding (State)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Read

type Parser = Parsec String ()

parseSession :: Parser (SessionData, Maybe CrawlLayer)
parseSession = liftM2 (,) parseSessionInfo $ (try $ Just <$> parseLayers) <|> (return Nothing)

skipNewLine :: a -> Parser a
skipNewLine val = endOfLine >> return val

parseSessionInfo :: Parser SessionData
parseSessionInfo = string "# Picture Monster" >> endOfLine >>
    string "## Session information" >> endOfLine >>
    SessionData <$> parseStartingUrls <*>
    parseInitialDepth <*>
    parseFileExtension <*>
    parseTargetDirectory >>=
    skipNewLine

parseStartingUrls :: Parser [URI]
parseStartingUrls = string "* Starting URLs:" >> endOfLine >> endBy1 parseStartingUrl endOfLine

parseStartingUrl :: Parser URI
parseStartingUrl = tab >> string "- " >> parseBackquotedUri

parseBackquotedUri :: Parser URI
parseBackquotedUri = parseBackquote >>= \s -> case (parseURI s) of
    Nothing     -> parserZero
    Just uri    -> return uri

parseBackquote :: Parser String
parseBackquote = between (char '`') (char '`') (many $ noneOf "`")

parseInitialDepth :: Parser SearchDepth
parseInitialDepth = (string "* Maximum search depth: ") >> parseDepth

parseDepth :: Parser SearchDepth
parseDepth = (many1 digit) >>= (\d -> case (readMaybe d) of
        Nothing     -> parserZero
        Just depth  -> return depth
    ) >>= skipNewLine

parseFileExtension :: Parser (Maybe Extension)
parseFileExtension = string "* Target file extension: " >>
    (Just <$> parseBackquote <|> (string "None" >> return Nothing)) >>= 
    skipNewLine

parseTargetDirectory :: Parser FilePath
parseTargetDirectory = string "* Target directory: " >> parseBackquote

parseLayers :: Parser CrawlLayer
parseLayers = layerUnion <$> ((string "## Crawling report") >> endOfLine >>
    some (try parseLayer))

layerUnion :: [CrawlLayer] -> CrawlLayer
layerUnion [] = Layer 0 $ State S.empty S.empty
layerUnion [x] = x
layerUnion (x:xs) = Layer (remainingDepth u) $ stateUnion (state x) (state u)
    where u = layerUnion xs

parseLayer :: Parser CrawlLayer
parseLayer = Layer <$> parseLayerHeader <*> parseLayerContents >>=
    \layer -> parseEnd "Layer complete." >> return layer

parseLayerHeader :: Parser SearchDepth
parseLayerHeader = string "### Layer " >> parseDepth

parseLayerContents :: Parser CrawlState
parseLayerContents = State <$> parseLayerSet "Links found" <*> parseLayerSet "Images found"

parseLayerSet :: String -> Parser (S.Set URI)
parseLayerSet header = string "#### " >> string header >> endOfLine >> parseUriList

parseUriList :: Parser (S.Set URI)
parseUriList = S.fromList <$> many (string "* " >> parseBackquotedUri >>= skipNewLine)

parseEnd :: String -> Parser ()
parseEnd msg = string "__" >> string msg >> string "__" >> endOfLine >> return ()

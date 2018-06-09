module PictureMonster.Parser where

import Control.Applicative hiding (many, (<|>))
import Network.URI
import PictureMonster.Data
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Read

type Parser = Parsec String ()

parseSessionInfo :: Parser SessionData
parseSessionInfo = string "# Picture Monster" >> endOfLine >>
    string "## Session information" >> endOfLine >>
    SessionData <$> parseStartingUrls <*>
    parseSearchDepth <*>
    parseFileExtension <*>
    parseTargetDirectory

parseStartingUrls :: Parser [URI]
parseStartingUrls = string "* Starting URLs:" >> endOfLine >> endBy1 parseStartingUrl endOfLine

parseStartingUrl :: Parser URI
parseStartingUrl = tab >> string "- " >> parseBackquote >>= \s -> case (parseURI s) of
    Nothing     -> parserZero
    Just uri    -> return uri

parseBackquote :: Parser String
parseBackquote = between (char '`') (char '`') (many $ noneOf "`")

parseSearchDepth :: Parser SearchDepth
parseSearchDepth = (string "* Maximum search depth: ") >> 
    (many1 digit) >>= (\d -> case (readMaybe d) of
        Nothing     -> parserZero
        Just depth  -> return depth
    ) >>= (\d -> endOfLine >> return d)

parseFileExtension :: Parser (Maybe Extension)
parseFileExtension = string "* Target file extension: " >>
    (Just <$> parseBackquote <|> (string "None" >> return Nothing)) >>= 
    (\ext -> endOfLine >> return ext)

parseTargetDirectory :: Parser FilePath
parseTargetDirectory = string "* Target directory: " >> parseBackquote

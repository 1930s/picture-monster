-- | The main program module.
-- Responsible for argument handling and parsing.
module Main where

import Control.Concurrent.ParallelIO.Global (stopGlobalPool)
import Control.Monad                        (void)
import Data.Semigroup                       ((<>))
import Network.URI                          (parseURI)
import Options.Applicative                  hiding (ParseError)
import PictureMonster.Crawler
import PictureMonster.Data
import PictureMonster.Downloader
import PictureMonster.Parser
import PictureMonster.Pooling
import PictureMonster.Serializer
import System.IO
import Text.Parsec                          (ParseError)
import Text.Read                            (readMaybe)

-- | Parser for positive integral values.
-- Succeeds if and only if the value read is a positive integer.
positiveReader :: (Read a, Integral a) => ReadM a
positiveReader = maybeReader readPositive
    where   readPositive :: (Read a, Integral a) => String -> Maybe a
            readPositive num = readMaybe num >>= (\x -> if x > 0 then Just x else Nothing)

-- | Parses the command-line option specifying the maximum number of outgoing connections.
totalConnLimit :: Parser ConnectionLimit
totalConnLimit = Limit <$> option positiveReader
    (long "limit" 
    <> short 'L'
    <> help "Total maximum number of connections. Must be a positive value.")

-- | Parses the command-line option specifying the maximum number of outgoing connections to a single host.
hostConnLimit :: Parser ConnectionLimit
hostConnLimit = Limit <$> option positiveReader
    (long "limit-per-host"
    <> short 'l'
    <> help "Maximum number of connections to single host. Must be a positive value.")

-- | Parses the command-line option specifying the maximum search depth used when crawling.
searchDepth :: Parser SearchDepth
searchDepth = option positiveReader
    (long "depth"
    <> short 'd'
    <> help "Maximum search depth during crawling. Must be a positive value.")

-- | Parses the command-line option specifying the extension of images to filter out.
imgExtension :: Parser (Maybe Extension)
imgExtension = (Just <$> option str
    (long "extension"
    <> short 'x'
    <> help "Image extension to filter out. Only image files with the supplied extension will be saved to disk."))
    <|> pure Nothing

-- | Parses the command-line option specifying the path to the target directory in which to save the downloaded files.
targetDirectory :: Parser FilePath
targetDirectory = option str
    (long "directory"
    <> short 'D'
    <> help "Target directory in which the images found should be saved. If omitted, images will be saved in the current working directory.")
    <|> pure "."

-- | Parses data concerning a single crawling session and returns them in a 'SessionData' instance.
sessionData :: Parser SessionData
sessionData = SessionData <$> some (argument (maybeReader parseURI) $
    metavar "URLS..."
    <> help "List of URLs to crawl in search of images.")
    <*> searchDepth
    <*> imgExtension
    <*> targetDirectory

-- | Runs the supplied parser and checks its value.
-- If the parser succeeds, its value is passed forward.
-- If the parser fails, the parser returned will return 'NoLimit'.
possibly :: Parser ConnectionLimit -- ^ The parser instance being transformed.
         -> Parser ConnectionLimit -- ^ A transformed parser instance, returning the original parser's value or 'NoLimit' in case of failure.
possibly limit = limit <|> pure NoLimit

-- | Parses the connection limits specified by the user in the command line.
connLimits :: Parser ConnectionLimits
connLimits = Limits <$> possibly totalConnLimit <*> possibly hostConnLimit

outputFilePath :: Parser FilePath
outputFilePath = option str
    (long "output"
    <> short 'o'
    <> help "Output Markdown file that will contain the crawling report. The file can be used to continue an interrupted session at a later date.")
    <|> pure "report.md"

-- | Parses arguments for the @new@ subcommand, which starts a new crawling session.
newSession :: Parser Command
newSession = NewSession <$> sessionData
    <*> connLimits
    <*> outputFilePath

inputFilePath :: Parser FilePath
inputFilePath = argument str
    (metavar "INPUT"
    <> help "Input Markdown file with the report from a previous session.")

-- | Parses arguments for the @continue@ subcommand, which continues a previous crawling session.
existingSession :: Parser Command
existingSession = ExistingSession <$> inputFilePath
    <*> connLimits

-- | Parses all possible subcommands (@new@, @list@ and @continue@).
commandParser :: Parser Command
commandParser = hsubparser (
        command "new" (info newSession (progDesc "Start new session"))
        <> command "continue" (info existingSession (progDesc "Continue previous session"))
        <> command "list" (info (pure ListSessions) (progDesc "Display all previous sessions"))
    )

-- | Used to display help text in case of parse failure.
opts :: ParserInfo Command
opts = info (commandParser <**> helper)
    (fullDesc
    <> header "picture-monster: Web crawler and image downloader utility")

-- | Runs the command specified by the user.
runCommand :: Command -- ^ Command to be executed.
           -> IO ()
runCommand (NewSession session limits path) = fromScratch path limits session >>= download session limits
runCommand (ExistingSession path limits) = parseReport <$> (readFile path) >>= (tryContinueSession path limits)
runCommand _ = error "not implemented"

tryContinueSession :: FilePath -> ConnectionLimits -> Either ParseError (SessionData, Maybe CrawlLayer) -> IO ()
tryContinueSession _ _ (Left error) = putStrLn "Report corrupted; cannot recover session"
tryContinueSession path limits (Right t@(session, _)) = (continueSession path limits t) >>= download session limits

continueSession :: FilePath -> ConnectionLimits -> (SessionData, Maybe CrawlLayer) -> IO [URIPool]
continueSession path limits (session, Nothing) = fromScratch path limits session
continueSession path limits (session, Just layer) = fromLayer limits session layer

fromScratch :: FilePath -> ConnectionLimits -> SessionData -> IO [URIPool]
fromScratch path limits session = withFile path WriteMode $ \handle ->
    serializeSession handle session >>
    (pool limits <$> crawl handle session limits)

fromLayer :: ConnectionLimits -> SessionData -> CrawlLayer -> IO [URIPool]
fromLayer limits session layer = pool limits <$> continueCrawl session limits layer

-- | The main entry point for the program.
main :: IO ()
main = execParser opts >>= runCommand >> stopGlobalPool

-- | The main program module.
-- Responsible for argument handling and parsing.
module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import Network.URI

-- | Identification number assigned to a previously started crawling session that has been interrupted.
-- Used to continue the session at a later data.
type SessionId = Int

-- | Represents a limit of HTTP connections that can be used during crawling.
data ConnectionLimit
    -- | Limited number of allowed HTTP connections.
    = Limit Int
    -- | Unlimited number of allowed HTTP connections.
    | NoLimit deriving Show

-- | Collects the connection limits imposed by the user.
data ConnectionLimits = Limits {
    -- | Limit of all outgoing HTTP connections.
    total :: ConnectionLimit,
    -- | Limit of outgoing HTTP connections to a single host.
    perHost :: ConnectionLimit
} deriving Show

-- | Represents the command chosen by the user.
data Command
    -- | Starts a new crawling session.
    -- The crawler will begin from a supplied list of URIs.
    -- The crawling and image downloading process will adhere to the specified connection limits.
    = NewSession [URI] ConnectionLimits
    -- | Continues a previously interrupted crawling session with the supplied ID.
    -- The crawling and image downloading process will adhere to the specified connection limits.
    | ExistingSession SessionId ConnectionLimits
    -- | Lists all interrupted crawling sessions with their IDs.
    | ListSessions deriving Show

-- | Parses the command-line option specifying the maximum number of outgoing connections.
totalConnLimit :: Parser ConnectionLimit
totalConnLimit = Limit <$> option auto
    (long "limit" 
    <> short 'L'
    <> help "Total maximum number of connections")

-- | Parses the command-line option specifying the maximum number of outgoing connections to a single host.
hostConnLimit :: Parser ConnectionLimit
hostConnLimit = Limit <$> option auto 
    (long "limit-per-host"
    <> short 'l'
    <> help "Maximum number of connections to single host")

-- | Runs the supplied parser and checks its value.
-- If the parser succeeds, its value is passed forward.
-- If the parser fails, the parser returned will return 'NoLimit'.
possibly :: Parser ConnectionLimit -- ^ The parser instance being transformed.
         -> Parser ConnectionLimit -- ^ A transformed parser instance, returning the original parser's value or 'NoLimit' in case of failure.
possibly limit = limit <|> pure NoLimit

-- | Parses the connection limits specified by the user in the command line.
connLimits :: Parser ConnectionLimits
connLimits = Limits <$> possibly totalConnLimit <*> possibly hostConnLimit

-- | Parses arguments for the @new@ subcommand, which starts a new crawling session.
newSession :: Parser Command
newSession = NewSession <$> some (argument (maybeReader parseURI) $ 
    metavar "URLS..."
    <> help "List of URLs to crawl in search of images.")
    <*> connLimits

-- | Parses arguments for the @continue@ subcommand, which continues a previous crawling session.
existingSession :: Parser Command
existingSession = ExistingSession <$> argument auto
    (metavar "SESSION_ID"
    <> help "ID number of existing session (use the `list` command to list all available sessions).")
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

-- | The main entry point for the program.
main :: IO ()
main = execParser opts >>= print

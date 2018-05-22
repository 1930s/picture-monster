-- | The main program module.
-- Responsible for argument handling and parsing.
module Main where

import Data.Semigroup ((<>))
import Network.URI (parseURI)
import Options.Applicative
import PictureMonster.Data

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

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Opts
    ( Actions(..)
    , opts
    , execParser
    , parseOpts
    ) where


import           Control.Monad
import           Data.Char
import qualified Data.Text           as T
import           Data.Time
import           Options.Applicative

import           Dialogue.Fields
import           Dialogue.Types

import           Types


-- * Option parsers

dbFileOpt :: Parser FilePath
dbFileOpt = strOption (  short 'D' <> long "db-file" <> metavar "DB_FILENAME"
                      <> value "dialogue.sqlite"
                      <> help "The database file name. Defaults to\
                              \ 'dialogue.sqlite'.")

dateOpt :: Parser UTCTime
dateOpt = option (parse =<< str)
                 (  short 'd' <> long "date" <> metavar "YYYY-MM-DD(THH:MM)?"
                 <> help "A date in ISO 8601 format with an optional time.")
    where
        parse :: String -> ReadM UTCTime
        parse s =   parseTimeM True defaultTimeLocale
                               (iso8601DateFormat (Just "%H:%M")) s
                <|> parseTimeM True defaultTimeLocale
                               (iso8601DateFormat Nothing) s

inputOpt :: Parser TextInput
inputOpt =   option (FileInput <$> str)
                    (  short 'i' <> long "input" <> metavar "FILENAME"
                    <> help "A filename to read input from. If both this and\
                            \ MESSAGE are missing, this will read from STDIN.")
         <|> option (RawInput . T.pack <$> str)
                    (  short 'm' <> long "message" <> metavar "MESSAGE"
                    <> help "A raw message to use as the input text.")
         <|> pure StdInput

inputFileOpt :: Parser FilePath
inputFileOpt = strOption (  short 'i' <> long "input" <> metavar "FILENAME"
                         <> help "A file name to read the input from.")

serviceOpt :: Parser Service
serviceOpt = option (parse . map toLower =<< str)
                    (  short 's' <> long "service" <> metavar "SERVICE_NAME"
                    <> help "The service to operate on. This is one of\
                            \ 'journal' or 'twitter'.\
                            \ You just need enough of this to be unique.")
    where
        parse :: MonadPlus m => String -> m Service
        parse name | take 1 name == "j" = return JournalService
                   | take 1 name == "t" = return TwitterService
                   | otherwise          = mzero

-- * Command parsers

initOpts :: Parser Actions
initOpts = Init <$> dbFileOpt

journalOpts :: Parser Actions
journalOpts = Journal <$> dbFileOpt <*> optional dateOpt <*> inputOpt

migrateOpts :: Parser Actions
migrateOpts =   Migrate <$> dbFileOpt <*> inputFileOpt <*> serviceOpt

-- * Bringing it all together

opts' :: Parser Actions
opts' = subparser
      (  command "init"    (info (helper <*> initOpts)
                            (progDesc "Default action and options."))
      <> command "journal" (info (helper <*> journalOpts)
                            (progDesc "Record a separate journal entry."))
      <> command "migrate" (info (helper <*> migrateOpts)
                            (progDesc "Import a data file from previous\
                                      \ iterations of this program."))
      )

opts :: ParserInfo Actions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "aggregating and bundling dialogues into an epub"
            <> header "dialogue - bundling dialogues")

parseOpts :: IO Actions
parseOpts = execParser opts

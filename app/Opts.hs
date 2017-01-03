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

outputFileOpt :: Parser FilePath
outputFileOpt = strOption (  short 'o' <> long "output" <> metavar "FILENAME"
                          <> help "A file name to write the output to.")

outputDirOpt :: Parser FilePath
outputDirOpt = strOption (  short 'o' <> long "output" <> metavar "DIRNAME"
                          <> help "A directory name to write the output to.")

serviceOpt :: Parser Service
serviceOpt = option (parse . map toLower =<< str)
                    (  short 's' <> long "service" <> metavar "SERVICE_NAME"
                    <> help "The service to operate on. This is one of\
                            \ 'adium', 'gdoc', 'google', 'journal', 'note', or\
                            \ 'twitter'. You just need enough of this to be\
                            \ unique.")
    where
        parse :: MonadPlus m => String -> m Service
        parse name | take 1 name == "a"  = return AdiumService
                   | take 2 name == "gd" = return GDocService
                   | take 2 name == "go" = return GoogleService
                   | take 1 name == "j"  = return JournalService
                   | take 1 name == "n"  = return NoteService
                   | take 1 name == "t"  = return TwitterService
                   | otherwise           = mzero

-- * Command parsers

initOpts :: Parser Actions
initOpts = Init <$> dbFileOpt

journalOpts :: Parser Actions
journalOpts = Journal <$> dbFileOpt <*> optional dateOpt <*> inputOpt

linkOpts :: Parser Actions
linkOpts = pure Links

mailOpts :: Parser Actions
mailOpts = Mail <$> dbFileOpt <*> inputFileOpt

migrateOpts :: Parser Actions
migrateOpts = Migrate <$> dbFileOpt <*> inputFileOpt <*> serviceOpt

updateOpts :: Parser Actions
updateOpts = Update <$> dbFileOpt <*> serviceOpt

statsOpts :: Parser Actions
statsOpts = Stats <$> dbFileOpt <*> outputFileOpt

publishOpts :: Parser Actions
publishOpts =   Publish
            <$> dbFileOpt
            <*> optional (strOption (  short 'c' <> long "cover-image"
                                    <> metavar "IMAGE_FILE"
                                    <> help "The file name for the cover\
                                            \ image."))
            <*> outputDirOpt

archiveOpts :: Parser Actions
archiveOpts = Archive <$> dbFileOpt <*> outputFileOpt

-- * Bringing it all together

opts' :: Parser Actions
opts' = subparser
      (  command "init"    (info (helper <*> initOpts)
                            (progDesc "Default action and options."))
      <> command "journal" (info (helper <*> journalOpts)
                            (progDesc "Record a separate journal entry."))
      <> command "links"   (info (helper <*> linkOpts)
                            (progDesc "Output an index of the links in a file."))
      <> command "mail"    (info (helper <*> mailOpts)
                            (progDesc "Import an mbox file."))
      <> command "migrate" (info (helper <*> migrateOpts)
                            (progDesc "Import a data file from previous\
                                      \ iterations of this program."))
      <> command "update"  (info (helper <*> updateOpts)
                            (progDesc "Update the database with new data\
                                      \ downloaded from the stream."))
      <> command "stats"   (info (helper <*> statsOpts)
                            (progDesc "Dump out statistics by month."))
      <> command "publish" (info (helper <*> publishOpts)
                            (progDesc "Write the Markdown and files to create\
                                      \ an EPUB for this directory"))
      <> command "archive" (info (helper <*> archiveOpts)
                            (progDesc "Archive the database's contents\
                                      \ into a JSON file."))
      )

opts :: ParserInfo Actions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "aggregating and bundling dialogues into an epub"
            <> header "dialogue - bundling dialogues")

parseOpts :: IO Actions
parseOpts = execParser opts

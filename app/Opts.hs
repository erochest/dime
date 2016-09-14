{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell   #-}


module Opts
    ( Actions(..)
    , opts
    , execParser
    , parseOpts
    ) where


import qualified Data.Text                as T
import           Data.Time
import           Options.Applicative

-- import           Development.Placeholders

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

-- * Command parsers

initOpts :: Parser Actions
initOpts = Init <$> dbFileOpt

journalOpts :: Parser Actions
journalOpts =   Journal
            <$> dbFileOpt
            <*> optional dateOpt
            <*> inputOpt

-- * Bringing it all together

opts' :: Parser Actions
opts' = subparser
      (  command "init"    (info (helper <*> initOpts)
                            (progDesc "Default action and options."))
      <> command "journal" (info (helper <*> journalOpts)
                            (progDesc "Record a separate journal entry."))
      )

opts :: ParserInfo Actions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "aggregating and bundling dialogues into an epub"
            <> header "dialogue - bundling dialogues")

parseOpts :: IO Actions
parseOpts = execParser opts

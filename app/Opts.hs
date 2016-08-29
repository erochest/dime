{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Opts
    ( Actions(..)
    , opts
    , execParser
    , parseOpts
    ) where


import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T
import           Options.Applicative

import           Types


configOpt :: Parser FilePath
configOpt = strOption (  short 'c' <> long "config" <> metavar "CONFIG_FILE"
                      <> help "Path to the configuration file with the\
                              \ login information.")

outputOpt :: Parser FilePath
outputOpt = strOption (  short 'o' <> long "output" <> metavar "OUTPUT_FILE"
                      <> help "The file to write back to.")

userOpt :: Parser T.Text
userOpt = option (T.pack <$> str)
                 (  short 'u' <> long "user" <> metavar "USERNAME"
                 <> help "The user name for a user to filter \
                         \interactions with."
                 )

tLoginOpts :: Parser Actions
tLoginOpts =   TLogin
           <$> configOpt
           <*> optional ((,) <$> option (B8.pack <$> str)
                                    (  short 'k' <> long "key" <> metavar "API_KEY"
                                    <> help "The access key for the Twitter API.")
                             <*> option (B8.pack <$> str)
                                    (  short 's' <> long "secret"
                                    <> metavar "API_SECRET"
                                    <> help "The access secret for the Twitter API.")
                        )

gLoginOpts :: Parser Actions
gLoginOpts = GLogin <$> configOpt

dmsOpts :: Parser Actions
dmsOpts = DMs
        <$> configOpt
        <*> optional userOpt
        <*> outputOpt

mergeOpts :: Parser Actions
mergeOpts =   Merge
          <$> strOption (  short 'd' <> long "dir" <> metavar "DIRECTORY"
                        <> help "The directory to walk over for input files.")
          <*> outputOpt

archiveOpts :: Parser Actions
archiveOpts =   Archive
            <$> configOpt
            <*> optional userOpt
            <*> strOption (  short 'a' <> long "archive-dir"
                          <> metavar "DIRNAME"
                          <> help "A directory of archived DMs.")

gmailOpts :: Parser Actions
gmailOpts =   Gmail
          <$> configOpt
          <*> strOption (  short 'u' <> long "user-index" <> metavar "JSON_FILE"
                        <> help "A JSON object mapping Twitter handles to\
                                \ email addresses.")
          <*> strOption (  short 'i' <> long "input" <> metavar "ARCHIVE_FILE"
                        <> help "An archive file to read Tweets from and\
                                \ insert into Gmail.")
          <*> option (T.pack <$> str)
                        (  short 'l' <> long "label" <> metavar "LABEL_NAME"
                        <> value "TWITTER"
                        <> help "The name of the label to file the Tweets in.\
                                \ Defaults to 'TWITTER'.")

tCountOpts :: Parser Actions
tCountOpts =   TCount
           <$> strOption (  short 'a' <> long "archive" <> metavar "FILENAME"
                         <> help "The file name to read Twitter DMs from for\
                                 \ counts.")
           <*> outputOpt
           <*> optional userOpt

opts' :: Parser Actions
opts' = subparser
    (  command "twitter-login" (info (helper <*> tLoginOpts)
                                    (progDesc "Set Twitter login keys in the\
                                              \ config file."))
    <> command "gmail-login" (info (helper <*> gLoginOpts)
                                    (progDesc "Set Gmail login keys in the\
                                              \ config file."))
    <> command "dms" (info (helper <*> dmsOpts)
                        (progDesc "Default action and options."))
    <> command "merge" (info (helper <*> mergeOpts)
                        (progDesc "Walk over a directory and merge all output\
                                  \ file."))
    <> command "archive" (info (helper <*> archiveOpts)
                            (progDesc "Pull down DMs, merge them with an\
                                      \ existing archive, and create a new\
                                      \ one. This is like a combination of\
                                      \ 'dms' and 'merge'."))
    <> command "gmail" (info (helper <*> gmailOpts)
                        (progDesc "Read DMs from an archive file and insert\
                                  \ into Gmail."))
    <> command "twitter-counts" (info (helper <*> tCountOpts)
                                    (progDesc "Count messages sent by month and\
                                              \ sender."))
    )

opts :: ParserInfo Actions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "This pulls down all the Twitter DMs from another person."
            <> header "dime - Archiving Twitter DMs.")

parseOpts :: IO Actions
parseOpts = execParser opts

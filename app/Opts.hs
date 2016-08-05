{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Opts
    ( Actions(..)
    , opts
    , execParser
    , parseOpts
    ) where


import qualified Data.Text           as T
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

loginOpts :: Parser Actions
loginOpts = Login <$> configOpt

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

opts' :: Parser Actions
opts' = subparser
    (  command "login" (info (helper <*> loginOpts)
                        (progDesc "Set login keys in the config file."))
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
    )

opts :: ParserInfo Actions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "This pulls down all the Twitter DMs from another person."
            <> header "dime - Archiving Twitter DMs.")

parseOpts :: IO Actions
parseOpts = execParser opts

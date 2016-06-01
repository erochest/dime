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

loginOpts :: Parser Actions
loginOpts = Login <$> configOpt

dmsOpts :: Parser Actions
dmsOpts = DMs
        <$> configOpt
        <*> optional (option (T.pack <$> str)
                             (  short 'u' <> long "user" <> metavar "USERNAME"
                             <> help "The user name for a user to filter \
                                     \interactions with."
                             )
                     )
        <*> outputOpt
        <*> strOption (  short 's' <> long "state-dir" <> metavar "DIRNAME"
                      <> value ".dime-state"
                      <> help "Path to hold working state. Default is \
                              \.dime-state")

opts' :: Parser Actions
opts' = subparser
    (  command "login" (info (helper <*> loginOpts)
                        (progDesc "Set login keys in the config file."))
    <> command "dms" (info (helper <*> dmsOpts)
                        (progDesc "Default action and options."))
    )

opts :: ParserInfo Actions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "This pulls down all the Twitter DMs from another person."
            <> header "dime - Archiving Twitter DMs.")

parseOpts :: IO Actions
parseOpts = execParser opts

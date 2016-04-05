{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Opts
    ( Actions(..)
    , opts
    , execParser
    , parseOpts
    ) where


-- import           Control.Monad       (mzero)
-- import qualified Data.List           as L
import qualified Data.Text           as T
import           Options.Applicative

import           Types


textOption :: Mod OptionFields T.Text -> Parser T.Text
textOption = option (T.pack <$> str)

configOpt :: Parser FilePath
configOpt = strOption (  short 'c' <> long "config" <> metavar "CONFIG_FILE"
                      <> help "Path to the configuration file with the\
                              \ login information.")

outputOpt :: Parser FilePath
outputOpt = strOption (  short 'o' <> long "output" <> metavar "OUTPUT_FILE"
                      <> help "The file to write back to.")

{-
 - inputOpt :: Parser FilePath
 - inputOpt = strOption (  short 'i' <> long "input" <> metavar "INPUT_FILE"
 -                      <> help "The input file to process.")
 -}

{-
 - inputsOpt :: Parser [FilePath]
 - inputsOpt = many (strArgument (  metavar "INPUT_FILES ..."
 -                               <> help "Input data files."))
 -}

loginOpts :: Parser Actions
loginOpts = Login <$> configOpt

dmsOpts :: Parser Actions
dmsOpts
  = DMs
    <$> configOpt
    <*> textOption (  short 'f' <> long "friend" <> metavar "TWITTER_USER"
                   <> help "The Twitter handle of the friend to scrape DMs of.")
    <*> outputOpt

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

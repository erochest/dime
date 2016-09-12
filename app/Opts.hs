{-# LANGUAGE LambdaCase #-}


module Opts
    ( Actions(..)
    , opts
    , execParser
    , parseOpts
    ) where


import           Options.Applicative

import           Types


dbFileOpt :: Parser FilePath
dbFileOpt = strOption (  short 'd' <> long "db-file" <> metavar "DB_FILENAME"
                      <> value "dialogue.sqlite"
                      <> help "The database file name. Defaults to\
                              \ 'dialogue.sqlite'.")

defaultOpts :: Parser Actions
defaultOpts = Init <$> dbFileOpt

opts' :: Parser Actions
opts' = subparser
      (  command "init" (info (helper <*> defaultOpts)
                          (progDesc "Default action and options."))
      )

opts :: ParserInfo Actions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "aggregating and bundling dialogues into an epub"
            <> header "dialogue - bundling dialogues")

parseOpts :: IO Actions
parseOpts = execParser opts

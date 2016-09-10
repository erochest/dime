{-# LANGUAGE LambdaCase #-}


module Opts
    ( Actions(..)
    , opts
    , execParser
    , parseOpts
    ) where


import           Options.Applicative

import           Types


defaultOpts :: Parser Actions
defaultOpts =   Login
            <$> strOption (  short 'a' <> long "auth" <> metavar "AUTH_FILE"
                          <> help "The file to put the authorization\
                                  \ information info."
                          )
            <*> many (option auto
                        (  metavar "SERVICE"
                        <> help "A service to process for this action. One of\
                                \ 'Twitter'."))

opts' :: Parser Actions
opts' = subparser
      (  command "login" (info (helper <*> defaultOpts)
                          (progDesc "Default action and options."))
      )

opts :: ParserInfo Actions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "aggregating and bundling dialogues into an epub"
            <> header "dialogue - bundling dialogues")

parseOpts :: IO Actions
parseOpts = execParser opts

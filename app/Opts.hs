{-# LANGUAGE LambdaCase #-}


module Opts
    ( Actions(..)
    , opts
    , execParser
    , parseOpts
    ) where


-- import           Control.Monad       (mzero)
-- import qualified Data.List           as L
-- import qualified Data.Text           as T
import           Options.Applicative

-- import           Dialog.Types

import           Types


-- textOption :: Mod OptionFields T.Text -> Parser T.Text
-- textOption = option (T.pack <$> str)

-- outputOpt :: Parser FilePath
-- outputOpt = strOption (  short 'o' <> long "output" <> metavar "OUTPUT_FILE"
--                       <> help "The file to write back to.")

-- inputOpt :: Parser FilePath
-- inputOpt = strOption (  short 'i' <> long "input" <> metavar "INPUT_FILE"
--                      <> help "The input file to process.")

-- inputsOpt :: Parser [FilePath]
-- inputsOpt = many (strArgument (  metavar "INPUT_FILES ..."
                              -- <> help "Input data files."))

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

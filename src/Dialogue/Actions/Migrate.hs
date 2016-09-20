{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Dialogue.Actions.Migrate where


import           Control.Error
import           Control.Exception.Safe
import           Control.Monad.IO.Class
import qualified Data.ByteString          as B
import           Data.Monoid
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO

import           Dialogue.Fields
import           Dialogue.Streams.Twitter
import           Dialogue.Types
import           Dialogue.Types.Dialogue


data MigrateException = MigrateException !T.Text
                      deriving (Show, Eq)
instance Exception MigrateException

migrateFile :: FilePath -> Service -> FilePath -> Script ()

migrateFile dbFile TwitterService inputFile = runDialogueS' (T.pack dbFile) $
    migrate inputFile =<< loadTwitter

migrateFile _ s _ = hoistEither
                  . Left
                  $ "No migration agent for " <> show s

migrate :: MessageStream ms mi => String -> ms -> Dialogue ()
migrate inputFile s =
    let name = streamName s
    in  case migrateMessages s of
            Nothing -> liftIO . TIO.putStrLn $ "No migration agent for " <> name
            Just f  ->
                f =<< liftIO (B.readFile inputFile)

{-# LANGUAGE OverloadedStrings #-}


module Dialogue.Actions.Journal where


import           Control.Error
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text              as T
import           Data.Time
import           Database.Persist

import           Dialogue.Models
import           Dialogue.Types
import           Dialogue.Utils


addJournal :: FilePath -> Maybe UTCTime -> TextInput -> Script ()
addJournal dbFile mSent input = runDialogueS' (T.pack dbFile) $ do
    date    <-  liftIO $ maybe getCurrentTime return mSent
    content <-  liftIO $ readInput input
    sender  <-  hoistE
            =<< fmap (note noPrimaryProfile . listToMaybe)
            (   liftSql
            $   selectKeysList [ProfilePrimary ==. True] [LimitTo 1]
            )
    void $ liftSql . insert $ Journal sender date content
    where
        noPrimaryProfile = toException $ ProfileException "No primary profile."

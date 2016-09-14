{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Actions where


import           Control.Error

import           Dialogue.Actions.Init
import           Dialogue.Actions.Journal

import           Types


action :: Actions -> Script ()

action Init{..}    = initialize initDbFile
action Journal{..} = addJournal journalDbFile journalDate journalInput

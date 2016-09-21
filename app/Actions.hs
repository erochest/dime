{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Actions where


import           Control.Error

import           Dialogue.Actions.Init
import           Dialogue.Actions.Journal
import           Dialogue.Actions.Migrate
import           Dialogue.Actions.Update

import           Types


action :: Actions -> Script ()

action Init{..}    = initialize initDbFile
action Journal{..} = addJournal journalDbFile journalDate journalInput
action Migrate{..} = migrateFile migrateDbFile migrateStream migrateInput
action Update{..}  = updateService updateDbFile updateStream

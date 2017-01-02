{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Actions where


import           Control.Error

import           Dialogue.Actions.Archive
import           Dialogue.Actions.Init
import           Dialogue.Actions.Journal
import           Dialogue.Actions.LinkIndex
import           Dialogue.Actions.Mail
import           Dialogue.Actions.Migrate
import           Dialogue.Actions.Publish
import           Dialogue.Actions.Stats
import           Dialogue.Actions.Update

import           Types


action :: Actions -> Script ()

action Archive{..} = archiveDb archiveDbFile archiveOutput
action Init{..}    = initialize initDbFile
action Journal{..} = addJournal journalDbFile journalDate journalInput
action Links       = indexLinks
action Mail{..}    = importMBox mailDbFile mailMBox
action Migrate{..} = migrateFile migrateDbFile migrateStream migrateInput
action Publish{..} = publishEpub publishDbFile publishOutDir
action Stats{..}   = generateStats statsDbFile statsOutput
action Update{..}  = updateService updateDbFile updateStream

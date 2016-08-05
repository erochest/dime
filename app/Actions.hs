{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Actions where


import           Control.Error

import           Dime.Actions.Archive
import           Dime.Actions.DMs
import           Dime.Actions.Login
import           Dime.Actions.Merge

import           Types


action :: Actions -> Script ()

action Login{..}   = loginTwitter loginConfig
action DMs{..}     = scrapeDMs dmsConfig dmsUserName dmsOutput
action Merge{..}   = mergeFiles mergeBaseDir mergeOutput
action Archive{..} = archiveDMs archiveConfig archiveUserName archiveDir

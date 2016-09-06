{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Actions where


import           Control.Error

import           Dime.Actions.Archive
import           Dime.Actions.Counts
import           Dime.Actions.DMs
import           Dime.Actions.Gmail
import           Dime.Actions.Login
import           Dime.Actions.Merge

import           Types


action :: Actions -> Script ()

action TLogin{..}  = loginTwitter tLoginConfig tLoginKeySecret
action GLogin{..}  = loginGmail gLoginConfig
action DMs{..}     = scrapeDMs dmsConfig dmsUserName dmsOutput
action Merge{..}   = mergeFiles mergeBaseDir mergeOutput
action Archive{..} = archiveDMs archiveConfig archiveUserName archiveDir
action Gmail{..}   = archiveGmail gmailConfig gmailUserIndex gmailArchiveFile
                                  gmailLabel gmailQueueDB
action TCount{..}  = twitterCounts tCountArchive tCountOutput tCountUserName

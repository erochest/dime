{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Actions where


import           Control.Error

import           Dime.Actions.DMs
import           Dime.Actions.Login

import           Types


action :: Actions -> Script ()

action Login{..} = loginTwitter loginConfig
action DMs{..}   = scrapeDMs dmsConfig dmsFriend dmsOutput

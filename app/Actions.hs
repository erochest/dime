{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Actions where


import           Control.Error

import           Dialogue.Actions.Init

import           Types


action :: Actions -> Script ()

action Init{..} = initialize initDbFile

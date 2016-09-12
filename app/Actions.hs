{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Actions where


import           Control.Error

import           Dialogue.Actions.Login

import           Types


action :: Actions -> Script ()

action Login{..} = login loginAuthFile loginService

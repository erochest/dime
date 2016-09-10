{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Actions where


import           Control.Error

import           Dialog.Actions.Login

import           Types


action :: Actions -> Script ()

action Login{..} = login loginAuthFile loginService

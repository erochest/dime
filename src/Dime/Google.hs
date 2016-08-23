{-# LANGUAGE OverloadedStrings #-}


module Dime.Google where


import           Control.Error
import           Control.Lens         hiding ((??))
import           Data.Aeson
import           Data.Aeson.Lens

import           Dime.Google.Network
import           Dime.Types


getUser :: Google UserName
getUser =   liftE
        .   (?? "Invalid user profile response.")
        .   preview (key "emailAddress". _String)
        =<< (getJSON url :: Google Value)
    where
        url = "https://www.googleapis.com/gmail/v1/users/me/profile"

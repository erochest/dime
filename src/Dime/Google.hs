{-# LANGUAGE OverloadedStrings #-}


module Dime.Google where


import           Control.Error
import           Control.Lens              hiding ((??))
import           Data.Aeson
import           Data.Aeson.Lens

import           Dime.Google.Network.Utils
import           Dime.Types


getUser :: Google UserName
getUser =   liftE
        .   (?? "Invalid user profile response.")
        .   preview (key "emailAddress". _String)
        =<< (getUncachedJSON url [] :: Google Value)
    where
        url = "/gmail/v1/users/me/profile"

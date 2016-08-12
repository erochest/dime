{-# LANGUAGE OverloadedStrings #-}


module Dime.Actions.Gmail where


import           Control.Error
import           Data.Monoid
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO

import           Dime.Google


archiveGmail :: FilePath -> FilePath -> FilePath -> T.Text -> Script ()
archiveGmail configFile _userIndex _archive _label = do
    userName <- runGoogle' configFile getUser
    scriptIO . TIO.putStrLn $ "You are: " <> userName

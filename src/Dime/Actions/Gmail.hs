{-# LANGUAGE OverloadedStrings #-}


module Dime.Actions.Gmail where


import           Control.Error
import           Control.Monad.IO.Class
import qualified Data.Text.IO           as TIO

import           Dime.Google
import qualified Dime.Google.Labels     as Labels
import           Dime.Google.Network
import qualified Dime.Google.Threads    as Threads
import           Dime.Google.Types


archiveGmail :: FilePath -> FilePath -> FilePath -> LabelName -> Script ()
archiveGmail configFile _userIndex _archive label = runGoogle' configFile $ do
    liftIO . TIO.putStrLn . mappend "You are: " =<< getUser

    twitterLabel <- Labels.ensure label
    liftIO $ print twitterLabel

    threads <- Threads.list [_labelId twitterLabel] Nothing Nothing Nothing
    liftIO $ print threads

    -- TODO: sketch out process

    undefined

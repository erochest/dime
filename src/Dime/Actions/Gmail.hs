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

    -- 1. get the thread id:
    --    * filter out full threads
    --    * get the latest thread's ID
    --    * get the last message in the thread or the last message in that label
    -- 2. read in the user index
    -- 3. read in the archive
    --    * based on the last message found, filter out the messages that have
    --      already been inserted
    -- 4. replace twitter handles with email addresses
    -- 5. format each tweet as rfc 822 email and create metadata
    --    * (get some SMS messages and look at their metadata)
    -- 6. chunk tweets into threads
    -- 7. submit thread
    --    * submit each message
    --    * carry the threadid through all messages

    undefined

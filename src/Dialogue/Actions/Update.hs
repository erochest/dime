{-# LANGUAGE OverloadedStrings #-}


module Dialogue.Actions.Update where


import           Control.Error
import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.Monoid
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

import           Dialogue.Fields
import           Dialogue.Types
import           Dialogue.Types.Dialogue
import           Dialogue.Streams.Twitter


data UpdateException = UpdateException !T.Text
                      deriving (Show, Eq)
instance Exception UpdateException

updateService :: FilePath -> Service -> Script ()
updateService dbFile service = runDialogueS' (T.pack dbFile) $
    case service of
        TwitterService -> loadTwitter >>= update'
        JournalService -> liftIO $ TIO.putStrLn "Nothing to update with journal."
        -- s -> throwD . UpdateException $ "Invalid service: " <> T.pack (show s)

update' :: MessageStream ms mi => ms -> Dialogue ()
update' ms = do
    active <- isActive ms
    if active
        then void $ downloadMessages ms
        else liftIO . TIO.putStrLn $ name <> " is not active."
    where
        name = streamName ms

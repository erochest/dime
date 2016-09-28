{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}


module Dialogue.Fields where


import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Data
import           Database.Persist.TH
import           GHC.Generics


data Service = TwitterService
             | GoogleService
             | AdiumService
             | NoteService
             | JournalService
             deriving (Eq, Show, Read, Data, Typeable, Generic)

instance ToJSON Service where
    toJSON AdiumService   = String "adium"
    toJSON GoogleService  = String "google"
    toJSON JournalService = String "journal"
    toJSON NoteService    = String "note"
    toJSON TwitterService = String "twitter"

instance FromJSON Service where
    parseJSON (String "adium"  ) = pure AdiumService
    parseJSON (String "google")  = pure GoogleService
    parseJSON (String "journal") = pure JournalService
    parseJSON (String "note"   ) = pure NoteService
    parseJSON (String "twitter") = pure TwitterService
    parseJSON _                  = mzero

derivePersistFieldJSON "Service"
$(makePrisms ''Service)

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
             | MailService
             | GDocService
             deriving (Eq, Show, Read, Data, Typeable, Generic)

instance ToJSON Service where
    toJSON AdiumService   = String "adium"
    toJSON GDocService    = String "gdoc"
    toJSON GoogleService  = String "google"
    toJSON JournalService = String "journal"
    toJSON MailService    = String "mail"
    toJSON NoteService    = String "note"
    toJSON TwitterService = String "twitter"

instance FromJSON Service where
    parseJSON (String "adium"  ) = pure AdiumService
    parseJSON (String "gdoc"   ) = pure GDocService
    parseJSON (String "google" ) = pure GoogleService
    parseJSON (String "journal") = pure JournalService
    parseJSON (String "mail"   ) = pure MailService
    parseJSON (String "note"   ) = pure NoteService
    parseJSON (String "twitter") = pure TwitterService
    parseJSON _                  = mzero

derivePersistFieldJSON "Service"
$(makePrisms ''Service)

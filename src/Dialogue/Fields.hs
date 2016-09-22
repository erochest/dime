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
             -- | Google
             -- | IRC
             | NoteService
             | JournalService
             deriving (Eq, Show, Read, Data, Typeable, Generic)

instance ToJSON Service where
    toJSON TwitterService = String "twitter"
    toJSON NoteService    = String "note"
    toJSON JournalService = String "journal"

instance FromJSON Service where
    parseJSON (String "twitter") = pure TwitterService
    parseJSON (String "note"   ) = pure NoteService
    parseJSON (String "journal") = pure JournalService
    parseJSON _                  = mzero

derivePersistFieldJSON "Service"
$(makePrisms ''Service)

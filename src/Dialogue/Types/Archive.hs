{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}


module Dialogue.Types.Archive where


import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Data
import           Database.Persist
import           GHC.Generics

import           Dialogue.Models
import           Dialogue.Utils


data Archive
    = Archive
    { _archiveProfiles    :: ![Entity Profile]
    , _archiveHandles     :: ![Entity Handle]
    , _archiveServices    :: ![Entity ServiceInfo]
    , _archiveAdium       :: ![Entity AdiumMessage]
    , _archiveGoogle      :: ![Entity GoogleMessage]
    , _archiveJournal     :: ![Entity Journal]
    , _archiveNoteMessage :: ![Entity NoteMessage]
    , _archiveTwitter     :: ![Entity TwitterMessage]
    } deriving (Typeable, Generic)
$(makeClassy ''Archive)

archiveOptions :: Options
archiveOptions = prefixOptions 8

instance ToJSON Archive where
    toJSON     = genericToJSON     archiveOptions
    toEncoding = genericToEncoding archiveOptions

instance FromJSON Archive where
    parseJSON = genericParseJSON archiveOptions

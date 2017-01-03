module Types where


import           Data.Time

import           Dialogue.Fields
import           Dialogue.Types


data Actions
    = Archive { archiveDbFile :: !FilePath
              , archiveOutput :: !FilePath
              }
    | Init    { initDbFile    :: !FilePath
              }
    | Journal { journalDbFile :: !FilePath
              , journalDate   :: !(Maybe UTCTime)
              , journalInput  :: !TextInput
              }
    | Links
    | Mail    { mailDbFile    :: !FilePath
              , mailMBox      :: !FilePath
              }
    | Migrate { migrateDbFile :: !FilePath
              , migrateInput  :: !FilePath
              , migrateStream :: !Service
              }
    | Publish { publishDbFile     :: !FilePath
              , publishCoverImage :: !(Maybe FilePath)
              , publishOutDir     :: !FilePath
              }
    | Stats   { statsDbFile   :: !FilePath
              , statsOutput   :: !FilePath
              }
    | Update  { updateDbFile  :: !FilePath
              , updateStream  :: !Service
              }
    deriving (Show, Eq)

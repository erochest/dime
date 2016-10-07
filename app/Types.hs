module Types where


import           Data.Time

import           Dialogue.Fields
import           Dialogue.Types


data Actions
    = Archive { archiveDbFile :: !FilePath
              , archiveOutput :: !FilePath
              }
    | Init    { initDbFile :: !FilePath
              }
    | Journal { journalDbFile :: !FilePath
              , journalDate   :: !(Maybe UTCTime)
              , journalInput  :: !TextInput
              }
    | Migrate { migrateDbFile :: !FilePath
              , migrateInput  :: !FilePath
              , migrateStream :: !Service
              }
    | Stats   { statsDbFile :: !FilePath
              , statsOutput :: !FilePath
              }
    | Update  { updateDbFile  :: !FilePath
              , updateStream  :: !Service
              }
    -- | Publish
    deriving (Show, Eq)

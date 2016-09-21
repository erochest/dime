module Types where


import           Data.Time

import           Dialogue.Fields
import           Dialogue.Types


data Actions
    = Init    { initDbFile :: !FilePath
              }
    | Journal { journalDbFile :: !FilePath
              , journalDate   :: !(Maybe UTCTime)
              , journalInput  :: !TextInput
              }
    | Migrate { migrateDbFile :: !FilePath
              , migrateInput  :: !FilePath
              , migrateStream :: !Service
              }
    | Update  { updateDbFile  :: !FilePath
              , updateStream  :: !Service
              }
    -- | Archive
    -- | Publish
    deriving (Show, Eq)

module Types where


import           Data.Time

import           Dialogue.Types


data Actions
    = Init    { initDbFile :: !FilePath
              }
    | Journal { journalDbFile :: !FilePath
              , journalDate   :: !(Maybe UTCTime)
              , journalInput  :: !TextInput
              }
    -- | Migrate
    -- | Update
    -- | Archive
    -- | Publish
    deriving (Show, Eq)

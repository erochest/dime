module Types where


data Actions
    = Init { initDbFile  :: !FilePath
           }
    -- | Journal
    -- | Migrate
    -- | Update
    -- | Archive
    -- | Publish
    deriving (Show, Eq)

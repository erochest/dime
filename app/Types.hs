module Types where


import           Dialogue.Types


data Actions
    = Login { loginAuthFile  :: !FilePath
            , loginService   :: ![Service]
            }
    -- | Import
    -- | Archive
    -- | Download
    -- | Export
    deriving (Show, Eq)

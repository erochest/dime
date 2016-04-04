module Types where


-- import           Dime.Types


data Actions
        = Default { defaultOutput :: !FilePath
                  , defaultInput  :: !FilePath
                  }
        deriving (Show, Eq)
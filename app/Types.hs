module Types where


import qualified Data.Text as T


data Actions
    = Login { loginConfig :: !FilePath
            }
    | DMs { dmsConfig   :: !FilePath
          , dmsUserName :: !(Maybe T.Text)
          , dmsOutput   :: !FilePath
          , dmsStateDir :: !FilePath
          }
    | Merge { mergeBaseDir :: !FilePath
            , mergeOutput  :: !FilePath
            }
    deriving (Show, Eq)

module Types where


import qualified Data.Text as T


data Actions
    = Login { loginConfig :: !FilePath
            }
    | DMs { dmsConfig   :: !FilePath
          , dmsUserName :: !(Maybe T.Text)
          , dmsOutput   :: !FilePath
          }
    | Merge { mergeBaseDir :: !FilePath
            , mergeOutput  :: !FilePath
            }
    | Archive { archiveConfig   :: !FilePath
              , archiveUserName :: !(Maybe T.Text)
              , archiveDir      :: !FilePath
              }
    deriving (Show, Eq)

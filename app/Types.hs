module Types where


import qualified Data.Text as T


data Actions
    = TLogin { tLoginConfig :: !FilePath
             }
    | GLogin { gLoginConfig :: !FilePath
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
    | Gmail { gmailConfig      :: !FilePath
            , gmailUserIndex   :: !FilePath
            , gmailArchiveFile :: !FilePath
            , gmailLabel       :: !T.Text
            }
    deriving (Show, Eq)

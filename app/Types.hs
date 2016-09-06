module Types where


import           Data.ByteString (ByteString)
import qualified Data.Text       as T


data Actions
    = TLogin { tLoginConfig    :: !FilePath
             , tLoginKeySecret :: !(Maybe (ByteString, ByteString))
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
            , gmailQueueDB     :: !FilePath
            }
    | TCount { tCountArchive  :: !FilePath
             , tCountOutput   :: !FilePath
             , tCountUserName :: !(Maybe T.Text)
             }
    deriving (Show, Eq)

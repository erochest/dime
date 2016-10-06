module Dialogue.Actions.Archive where


import           Control.Error
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text               as T
import           Database.Persist

import           Dialogue.Types.Archive
import           Dialogue.Types.Dialogue


archiveDb :: FilePath -> FilePath -> Script ()
archiveDb dbFile outFile = runDialogueS' (T.pack dbFile) $ do
    a <-  liftSql
      $   Archive
      <$> selectList [] []
      <*> selectList [] []
      <*> selectList [] []
      <*> selectList [] []
      <*> selectList [] []
      <*> selectList [] []
      <*> selectList [] []
      <*> selectList [] []
    liftIO $ BL.writeFile outFile $ encode a

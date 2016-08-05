{-# LANGUAGE TupleSections #-}


module Dime.Actions.Archive where


import           Control.Error
import           Data.Aeson           (encode)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import           Data.Time
import           System.FilePath

import           Dime.Twitter
import           Dime.Utils


archiveDMs :: FilePath -> Maybe T.Text -> FilePath -> Script ()
archiveDMs configFile mUserName archiveDir = do
    now <-  formatTime defaultTimeLocale "%Y%m%d-%H%M%S"
        <$> scriptIO getCurrentTime
    scriptIO
        . B.writeFile (archiveDir </> ("archive-" ++ now ++ ".json"))
        . encode
        . sortByDate
        . dedupDM
        $ (++) <$> (   fmap (filterByUser mUserName)
                   .   downloadDMs
                   =<< readTWInfo configFile
                   )
               <*> readDMDirTree archiveDir

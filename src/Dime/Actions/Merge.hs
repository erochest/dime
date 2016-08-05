

module Dime.Actions.Merge where


import           Control.Error
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.List            as L
import           Data.Ord
import           System.FilePath
import           Web.Twitter.Types

import           Dime.Utils


mergeFiles :: FilePath -> FilePath -> Script ()
mergeFiles inputDir outputFile =
    scriptIO . B.writeFile outputFile
    .   encode . L.sortBy (comparing dmCreatedAt) . dedupDM . concat
    =<< mapM (hoistEither . eitherDecode' <=< scriptIO . B.readFile)
    .   filter ((== ".json") . takeExtension)
    =<< walkDirectoryTree inputDir



module Dime.Actions.Merge where


import           Control.Error
import           Data.Aeson
import qualified Data.ByteString.Lazy as B

import           Dime.Twitter
import           Dime.Utils


mergeFiles :: FilePath -> FilePath -> Script ()
mergeFiles inputDir outputFile =
    scriptIO . B.writeFile outputFile . encode . sortByDate . dedupDM
        =<< readDMDirTree inputDir

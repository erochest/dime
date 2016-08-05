{-# LANGUAGE OverloadedStrings #-}


module Dime.Actions.DMs where


import           Control.Error
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

import           Dime.Twitter
import           Dime.Utils


scrapeDMs :: FilePath -> Maybe T.Text -> FilePath -> Script ()
scrapeDMs configFile mUserName output =
    scriptIO
        .   BL.writeFile output
        .   encode
        .   sortByDate
        .   filterByUser mUserName
        =<< downloadDMs
        =<< readTWInfo configFile

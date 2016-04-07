module Dime.Config where


import           Control.Error
import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL

import           Dime.Types


readConfig :: FilePath -> Script LoginInfo
readConfig configFile =
    hoistEither . eitherDecodeStrict'
  =<< scriptIO (BS.readFile configFile)


writeConfig :: FilePath -> LoginInfo -> Script ()
writeConfig configFile config =
    scriptIO . BL.writeFile configFile $ encode config


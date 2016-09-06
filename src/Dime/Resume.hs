

module Dime.Resume where


import           Data.Aeson
import           Database.Persist
import           Network.OAuth.OAuth2

import           Dime.Google.Types
import           Dime.Types


queueURL :: URI -> Google (Entity DownloadCache)
queueURL = undefined

nextURL :: Google (Entity DownloadCache)
nextURL = undefined

downloadURL :: ToJSON a
            => Entity ArchiveSession -> Entity DownloadCache -> a
            -> Google (Entity DownloadCache)
downloadURL = undefined

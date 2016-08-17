{-# LANGUAGE OverloadedStrings #-}


module Dime.Google.Threads where


import           Data.Maybe
import           Data.Monoid
import           Data.Text.Encoding

import           Dime.Google.Network
import           Dime.Google.Types
import           Dime.Types


get :: ThreadId -> Google Thread
get tId = getJSON $  "https://www.googleapis.com/gmail/v1/users/me/threads/"
                  <> encodeUtf8 tId

list :: [LabelId] -> Maybe Int -> Maybe PageToken -> Maybe Query
     -> Google ThreadList
list labelIds maxResults pageToken q =
    getJSON' url
        $ catMaybes [ maybeParam "maxResults" maxResults
                    , maybeParam "pageToken"  pageToken
                    , maybeParam "q"          q
                    , maybeList  "labelIds"   labelIds
                    ]
    where
        url = "https://www.googleapis.com/gmail/v1/users/me/threads"

{-# LANGUAGE OverloadedStrings #-}


module Dime.Google.Threads where


import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence       as Seq
import           Data.Text.Encoding

import           Dime.Google.Network
import           Dime.Google.Types


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

listAll :: [LabelId] -> Maybe Query -> Google [Thread]
listAll labelIds q = toList <$> go Nothing
    where
        go pt = do
            tl <- list labelIds Nothing pt q
            let ts = Seq.fromList . fold $ _threadsThreads tl
            mappend ts <$> maybe (return mempty) (go . Just)
                                 (_threadsNextPageToken tl)

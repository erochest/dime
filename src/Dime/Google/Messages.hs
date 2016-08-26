{-# LANGUAGE OverloadedStrings #-}


module Dime.Google.Messages where


import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable
import qualified Data.HashSet         as S
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence        as Seq
import           Data.Text.Encoding
import           Network.Wreq         hiding (get, post)

import           Dime.Google.DSL
import qualified Dime.Google.DSL      as DSL
import           Dime.Google.Network
import           Dime.Google.Types


get :: MessageId -> GoogleAction Message
get mId = DSL.get url []
    where
        -- url = "https://www.googleapis.com/gmail/v1/users/me/messages/"
        url = "/gmail/v1/users/me/messages/" <> encodeUtf8 mId

insert :: MessageInfo -> RawMessage -> GoogleAction Message
insert m raw = post uri [("uploadType", ["multipart"])]
             . MultiRelated
             $ [ partBS "metadata" (BL.toStrict $ encode m')
                    & partContentType ?~ "application/json"
               , partBS "raw" (unBytes $ _rawMessageRaw raw)
                    & partContentType ?~ "message/rfc822"
               ]
    where
        uri = "/upload/gmail/v1/users/me/messages"
        m'  = m & messageInfoThreadId %~ (_rawMessageThreadId raw <|>)
                & messageInfoLabelIds %~ ( fmap S.toList
                                         . (   foldl' (flip S.insert)
                                           .   S.fromList
                                           <$> _rawMessageLabelIds raw
                                           <*>))

list :: [LabelId] -> Maybe Int -> Maybe PageToken -> Maybe Query
     -> GoogleAction MessageList
list labelIds maxResults pageToken q =
    DSL.get url $ catMaybes [ maybeParam "maxResults" maxResults
                            , maybeParam "pageToken"  pageToken
                            , maybeParam "q"          q
                            , maybeList  "labelIds"   labelIds
                            ]
    where
        url = "/gmail/v1/users/me/messages"

listAll :: [LabelId] -> Maybe Query -> Google [MessageShort]
listAll labelIds q = singleActions $ toList <$> go Nothing
    where
        go pt = do
            ml <- list labelIds Nothing pt q
            let ms = Seq.fromList $ _messagesMessages ml
            mappend ms <$> maybe (return mempty) (go . Just)
                                 (_messagesNextPageToken ml)

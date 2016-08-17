{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}


module Dime.Google.Types where


import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString                       (ByteString)
import           Data.Char
import           Data.Data
import           Data.Monoid                           ((<>))
import qualified Data.Text                             as T
import           Data.Text.Encoding
import           GHC.Generics
import           Network.HTTP.Client.Internal          hiding ((<>))
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Types.Header             (hContentType)
import           Network.HTTP.Types.Method
import           Network.Wreq.Types                    hiding (Options, Payload)


googleOptions :: Int -> Options
googleOptions n = defaultOptions
                { fieldLabelModifier = lowerFirst . drop n
                }
    where
        lowerFirst []     = []
        lowerFirst (x:xs) = toLower x : xs

type LabelId      = T.Text
type LabelName    = T.Text
type ThreadId     = T.Text
type PageToken    = T.Text
type Query        = T.Text
type MessageId    = T.Text
type MessageRaw   = T.Text
type HistoryId    = Integer
type AttachmentId = T.Text

data MessageListVisibility = ShowMessage | HideMessage
                           deriving (Show, Eq, Data, Typeable, Generic)
$(makePrisms ''MessageListVisibility)

instance ToJSON MessageListVisibility where
    toJSON ShowMessage = String "show"
    toJSON HideMessage = String "hide"

instance FromJSON MessageListVisibility where
    parseJSON (String "show") = return ShowMessage
    parseJSON (String "hide") = return HideMessage
    parseJSON _               = mzero

data LabelListVisibility = ShowLabel | HideLabel | ShowIfUnread
                         deriving (Show, Eq, Data, Typeable, Generic)
$(makePrisms ''LabelListVisibility)

instance ToJSON LabelListVisibility where
    toJSON ShowLabel    = String "labelShow"
    toJSON HideLabel    = String "labelHide"
    toJSON ShowIfUnread = String "labelShowIfUnread"

instance FromJSON LabelListVisibility where
    parseJSON (String "labelShow")         = return ShowLabel
    parseJSON (String "labelHide")         = return HideLabel
    parseJSON (String "labelShowIfUnread") = return ShowIfUnread
    parseJSON _                            = mzero

data LabelType = System | User
               deriving (Show, Eq, Data, Typeable, Generic)
$(makePrisms ''LabelType)

instance ToJSON LabelType where
    toJSON System = String "system"
    toJSON User   = String "user"

instance FromJSON LabelType where
    parseJSON (String "system") = return System
    parseJSON (String "user")   = return User
    parseJSON _                 = mzero

data Label
    = Label
    { _labelId                    :: !LabelId
    , _labelName                  :: !LabelName
    , _labelType                  :: !(Maybe LabelType)
    , _labelMessageListVisibility :: !(Maybe MessageListVisibility)
    , _labelLabelListVisibility   :: !(Maybe LabelListVisibility)
    , _labelMessagesTotal         :: !(Maybe Int)
    , _labelMessagesUnread        :: !(Maybe Int)
    , _labelThreadsTotal          :: !(Maybe Int)
    , _labelThreadsUnread         :: !(Maybe Int)
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''Label)

instance ToJSON Label where
    toJSON     = genericToJSON     (googleOptions 6)
    toEncoding = genericToEncoding (googleOptions 6)

instance FromJSON Label where
    parseJSON = genericParseJSON (googleOptions 6)

data Labels
    = Labels
    { _labelsLabels :: [Label]
    } deriving (Show, Eq, Data, Typeable, Generic)

instance ToJSON Labels where
    toJSON     = genericToJSON     (googleOptions 7)
    toEncoding = genericToEncoding (googleOptions 7)

instance FromJSON Labels where
    parseJSON = genericParseJSON (googleOptions 7)

data LabelInfo
    = LabelInfo
    { _labelInfoName                  :: !LabelName
    , _labelInfoLabelListVisibility   :: !LabelListVisibility
    , _labelInfoMessageListVisibility :: !MessageListVisibility
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''LabelInfo)

instance ToJSON LabelInfo where
    toJSON     = genericToJSON     (googleOptions 10)
    toEncoding = genericToEncoding (googleOptions 10)

instance FromJSON LabelInfo where
    parseJSON = genericParseJSON (googleOptions 10)

newtype JSBytes = JSBytes { unBytes :: ByteString }
                deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''JSBytes)

instance ToJSON JSBytes where
    toJSON = String . decodeUtf8 . unBytes

instance FromJSON JSBytes where
    parseJSON (String s) = return . JSBytes $ encodeUtf8 s
    parseJSON _          = mzero

data Header
    = Header
    { _headerName  :: !T.Text
    , _headerValue :: !T.Text
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''Header)

instance ToJSON Header where
    toJSON     = genericToJSON     (googleOptions 7)
    toEncoding = genericToEncoding (googleOptions 7)

instance FromJSON Header where
    parseJSON = genericParseJSON (googleOptions 7)

data Attachment
    = Attachment
    { _attachmentId   :: !AttachmentId
    , _attachmentSize :: !Int
    , _attachmentData :: !JSBytes
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''Attachment)

instance ToJSON Attachment where
    toJSON     = genericToJSON     (googleOptions 11)
    toEncoding = genericToEncoding (googleOptions 11)

instance FromJSON Attachment where
    parseJSON = genericParseJSON (googleOptions 11)

data AttachmentInfo
    = AttachmentInfo
    { _attachmentInfoSize :: !(Maybe Int)
    , _attachmentInfoData :: !JSBytes
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''AttachmentInfo)

instance ToJSON AttachmentInfo where
    toJSON     = genericToJSON     (googleOptions 15)
    toEncoding = genericToEncoding (googleOptions 15)

instance FromJSON AttachmentInfo where
    parseJSON = genericParseJSON (googleOptions 15)

type MessagePart = JSBytes

data Payload
    = Payload
    { _payloadPartId   :: !T.Text
    , _payloadMimeType :: !T.Text
    , _payloadFilename :: !T.Text
    , _payloadHeaders  :: ![Header]
    , _payloadBody     :: ![Attachment]
    , _payloadParts    :: ![MessagePart]
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''Payload)

instance ToJSON Payload where
    toJSON     = genericToJSON     (googleOptions 8)
    toEncoding = genericToEncoding (googleOptions 8)

instance FromJSON Payload where
    parseJSON = genericParseJSON (googleOptions 8)

data PayloadInfo
    = PayloadInfo
    { _payloadInfoMimeType :: !T.Text
    , _payloadInfoFilename :: !T.Text
    , _payloadInfoHeaders  :: ![Header]
    , _payloadInfoBody     :: ![AttachmentInfo]
    , _payloadInfoParts    :: ![MessagePart]
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''PayloadInfo)

instance ToJSON PayloadInfo where
    toJSON     = genericToJSON     (googleOptions 12)
    toEncoding = genericToEncoding (googleOptions 12)

instance FromJSON PayloadInfo where
    parseJSON = genericParseJSON (googleOptions 12)

data RawMessage
    = RawMessage
    { _rawMessageRaw      :: !JSBytes
    , _rawMessageLabelIds :: !(Maybe [LabelId])
    , _rawMessageThreadId :: !(Maybe ThreadId)
    } deriving (Show, Eq, Data, Typeable, Generic)

instance ToJSON RawMessage where
    toJSON     = genericToJSON     (googleOptions 11)
    toEncoding = genericToEncoding (googleOptions 11)

instance FromJSON RawMessage where
    parseJSON = genericParseJSON (googleOptions 11)

data Message
    = Message
    { _messageId           :: !MessageId
    , _messageThreadId     :: !ThreadId
    , _messageLabelIds     :: ![LabelId]
    , _messageSnippet      :: !T.Text
    , _messageHistoryId    :: !HistoryId
    , _messageInternalDate :: !Int
    , _messagePayload      :: !Payload
    , _messageSizeEstimate :: !Int
    , _messageRaw          :: !JSBytes
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''Message)

instance ToJSON Message where
    toJSON     = genericToJSON     (googleOptions 8)
    toEncoding = genericToEncoding (googleOptions 8)

instance FromJSON Message where
    parseJSON = genericParseJSON (googleOptions 8)

data MessageInfo
    = MessageInfo
    { _messageInfoThreadId     :: !(Maybe ThreadId)
    , _messageInfoLabelIds     :: !(Maybe [LabelId])
    , _messageInfoPayload      :: !PayloadInfo
    , _messageInfoRaw          :: !JSBytes
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''MessageInfo)

instance ToJSON MessageInfo where
    toJSON     = genericToJSON     (googleOptions 12)
    toEncoding = genericToEncoding (googleOptions 12)

instance FromJSON MessageInfo where
    parseJSON = genericParseJSON (googleOptions 12)

data Thread
    = Thread
    { _threadId        :: !ThreadId
    , _threadSnippet   :: !T.Text
    , _threadHistoryId :: !HistoryId
    , _threadMessages  :: ![Message]
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''Thread)

instance ToJSON Thread where
    toJSON     = genericToJSON     (googleOptions 7)
    toEncoding = genericToEncoding (googleOptions 7)

instance FromJSON Thread where
    parseJSON = genericParseJSON (googleOptions 7)

data MessageList
    = MessageList
    { _messagesMessages           :: ![Message]
    , _messagesNextPageToken      :: !PageToken
    , _messagesResultSizeEstimate :: !Int
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''MessageList)

instance ToJSON MessageList where
    toJSON     = genericToJSON     (googleOptions 9)
    toEncoding = genericToEncoding (googleOptions 9)

instance FromJSON MessageList where
    parseJSON = genericParseJSON (googleOptions 9)

data ThreadList
    = ThreadList
    { _threadsThreads            :: !(Maybe [Thread])
    , _threadsNextPageToken      :: !(Maybe PageToken)
    , _threadsResultSizeEstimate :: !Int
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''ThreadList)

instance ToJSON ThreadList where
    toJSON     = genericToJSON     (googleOptions 8)
    toEncoding = genericToEncoding (googleOptions 8)

instance FromJSON ThreadList where
    parseJSON = genericParseJSON (googleOptions 8)

newtype MultipartRelated = MultiRelated { unRelate :: [Part] }
                         deriving (Show, Typeable, Generic)
$(makeLenses ''MultipartRelated)

instance Postable MultipartRelated where
    -- copied and pasted from Network.HTTP.Client.MultipartFormData.
    -- Yuck.
    -- Brittle.
    postPayload (MultiRelated ps) req = do
        boundary <- webkitBoundary
        body <- renderParts boundary ps
        return $ req
               { method = methodPost
               , requestHeaders =
                   (hContentType , "multipart/related; boundary=" <> boundary)
                    : filter (\(x, _) -> x /= hContentType) (requestHeaders req)
               , requestBody = body
               }

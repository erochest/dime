{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}


module Dime.Google.Types where


import           Control.Arrow                         ((&&&))
import           Control.Error                         (ExceptT (..), Script,
                                                        throwE)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Free
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor                        (first)
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString.Lazy.Char8            as L8
import           Data.Char
import           Data.Data
import           Data.Monoid                           ((<>))
import qualified Data.Text                             as T
import           Data.Text.Encoding
import           Database.Persist.Sql
import           GHC.Generics
import           Network.HTTP.Client.Internal          hiding ((<>))
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Types.Header             (hContentType)
import           Network.HTTP.Types.Method
import           Network.OAuth.OAuth2
import           Network.Wreq.Types                    hiding (Options, Payload)


-- * Google monad

data GoogleData
    = GoogleData
    { _gdManager     :: !Manager
    , _gdAccessToken :: !AccessToken
    , _gdSqlBackend  :: !SqlBackend
    } deriving (Typeable, Generic)
$(makeLenses ''GoogleData)

newtype GoogleT m a
    = GoogleT { unGoogleT :: ReaderT GoogleData (LoggingT (ExceptT String m)) a }
    deriving ( Functor, Applicative, Monad, MonadReader GoogleData
             , MonadIO, MonadLogger, Generic
             )

type Google = GoogleT IO

instance MonadTrans GoogleT where
    lift = GoogleT . lift . lift . lift

instance MonadThrow m => MonadThrow (GoogleT m) where
    throwM = GoogleT . lift . lift . throwE . show

instance MonadTransControl GoogleT where
    type StT GoogleT a = StT LoggingT a
    liftWith runG = GoogleT $ liftWith $ \runR ->
                              liftWith $ \runL ->
                              liftWith $ \runE ->
                                runG $ (either fail return =<<)
                                     . runE . runL . runR . unGoogleT
    restoreT = GoogleT . restoreT . restoreT . restoreT . fmap Right

instance MonadBase b m => MonadBase b (GoogleT m) where
    liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (GoogleT m) where
    type StM (GoogleT m) a = ComposeSt GoogleT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

runGoogle :: Manager -> AccessToken -> SqlBackend -> Google a -> Script a
runGoogle m t s = runStderrLoggingT . runGoogleL m t s

runGoogleL :: Manager -> AccessToken -> SqlBackend -> Google a
           -> LoggingT (ExceptT String IO) a
runGoogleL m t s g = runReaderT (unGoogleT g) (GoogleData m t s)

currentManager :: Monad m => GoogleT m Manager
currentManager = view gdManager

currentAccessToken :: Monad m => GoogleT m AccessToken
currentAccessToken = view gdAccessToken

currentSqlBackend :: Monad m => GoogleT m SqlBackend
currentSqlBackend = view gdSqlBackend

currentManagerToken :: Monad m => GoogleT m (Manager, AccessToken)
currentManagerToken = asks (_gdManager &&& _gdAccessToken)

liftSql :: ReaderT SqlBackend (LoggingT (ExceptT String m)) a -> GoogleT m a
liftSql r =
    GoogleT $ withReaderT (view gdSqlBackend) r

liftE :: Script a -> Google a
liftE = GoogleT . lift . lift

liftG :: IO (OAuth2Result a) -> Google a
liftG = liftE . liftSG

liftSG :: IO (OAuth2Result a) -> Script a
liftSG = ExceptT . fmap (first L8.unpack)

-- * Google DSL

type GetParam = (T.Text, [T.Text])

-- TODO: Can I add a phantom type that will allow me to say when batch actions
-- aren't appropriate? How would combining these work?
-- TODO: Or even better, to trace dependencies between calls automatically and
-- use that to know when actions can be batched?
data GoogleActionF a where
    GGet  :: FromJSON response
          => URI -> [GetParam]         -> (response -> a) -> GoogleActionF a
    GPost :: (FromJSON response, Postable post)
          => URI -> [GetParam] -> post -> (response -> a) -> GoogleActionF a

instance Functor GoogleActionF where
    fmap f (GGet  uri ps   r) = GGet  uri ps   $ f . r
    fmap f (GPost uri ps p r) = GPost uri ps p $ f . r

type GoogleAction n = Free GoogleActionF n

-- * Google data types

-- ** Utilities

googleOptions :: Int -> Options
googleOptions n = defaultOptions
                { fieldLabelModifier = lowerFirst . drop n
                }
    where
        lowerFirst []     = []
        lowerFirst (x:xs) = toLower x : xs

-- ** Type aliases

type LabelId      = T.Text
type LabelName    = T.Text
type ThreadId     = T.Text
type PageToken    = T.Text
type Query        = T.Text
type MessageId    = T.Text
type MessageRaw   = T.Text
type HistoryId    = T.Text
type AttachmentId = T.Text

-- ** Types
-- *** JSBytes (ByteString wrapper)

newtype JSBytes = JSBytes { unBytes :: ByteString }
                deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''JSBytes)

instance ToJSON JSBytes where
    toJSON = String . decodeUtf8 . unBytes

instance FromJSON JSBytes where
    parseJSON (String s) = return . JSBytes $ encodeUtf8 s
    parseJSON _          = mzero

-- *** Label

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

-- *** Header

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

headerToParam :: Header -> GetParam
headerToParam (Header n v) = (n, [v])

-- *** Attachment

data Attachment
    = Attachment
    { _attachmentId   :: !(Maybe AttachmentId)
    , _attachmentSize :: !Int
    , _attachmentData :: !(Maybe JSBytes)
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

-- *** Payload

data Payload
    = Payload
    { _payloadPartId   :: !(Maybe T.Text)
    , _payloadMimeType :: !T.Text
    , _payloadFilename :: !T.Text
    , _payloadHeaders  :: !(Maybe [Header])
    , _payloadBody     :: !Attachment
    , _payloadParts    :: !(Maybe [Payload])
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
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''PayloadInfo)

instance ToJSON PayloadInfo where
    toJSON     = genericToJSON     (googleOptions 12)
    toEncoding = genericToEncoding (googleOptions 12)

instance FromJSON PayloadInfo where
    parseJSON = genericParseJSON (googleOptions 12)

-- *** RawMessage

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

-- *** Messages

data MessageShort
    = MessageShort
    { _messageShortId       :: !MessageId
    , _messageShortThreadId :: !ThreadId
    } deriving (Show, Eq, Data, Typeable, Generic)

instance ToJSON MessageShort where
    toJSON     = genericToJSON     (googleOptions 13)
    toEncoding = genericToEncoding (googleOptions 13)

instance FromJSON MessageShort where
    parseJSON = genericParseJSON (googleOptions 13)

data Message
    = Message
    { _messageId           :: !MessageId
    , _messageThreadId     :: !ThreadId
    , _messageLabelIds     :: ![LabelId]
    , _messageSnippet      :: !(Maybe T.Text)
    , _messageHistoryId    :: !HistoryId
    , _messageInternalDate :: !T.Text
    , _messagePayload      :: !Payload
    , _messageSizeEstimate :: !Int
    , _messageRaw          :: !(Maybe JSBytes)
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''Message)

instance ToJSON Message where
    toJSON     = genericToJSON     (googleOptions 8)
    toEncoding = genericToEncoding (googleOptions 8)

instance FromJSON Message where
    parseJSON = genericParseJSON (googleOptions 8)

data MessageInfo
    = MessageInfo
    { _messageInfoThreadId :: !(Maybe ThreadId)
    , _messageInfoLabelIds :: !(Maybe [LabelId])
    , _messageInfoPayload  :: !PayloadInfo
    , _messageInfoRaw      :: !(Maybe JSBytes)
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''MessageInfo)

instance ToJSON MessageInfo where
    toJSON     = genericToJSON     (googleOptions 12)
    toEncoding = genericToEncoding (googleOptions 12)

instance FromJSON MessageInfo where
    parseJSON = genericParseJSON (googleOptions 12)

data MessageList
    = MessageList
    { _messagesMessages           :: ![MessageShort]
    , _messagesNextPageToken      :: !(Maybe PageToken)
    , _messagesResultSizeEstimate :: !Int
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''MessageList)

instance ToJSON MessageList where
    toJSON     = genericToJSON     (googleOptions 9)
    toEncoding = genericToEncoding (googleOptions 9)

instance FromJSON MessageList where
    parseJSON = genericParseJSON (googleOptions 9)

-- *** Thread

data Thread
    = Thread
    { _threadId        :: !ThreadId
    , _threadSnippet   :: !T.Text
    , _threadHistoryId :: !HistoryId
    , _threadMessages  :: !(Maybe [Message])
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''Thread)

instance ToJSON Thread where
    toJSON     = genericToJSON     (googleOptions 7)
    toEncoding = genericToEncoding (googleOptions 7)

instance FromJSON Thread where
    parseJSON = genericParseJSON (googleOptions 7)

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

-- *** MultipartRelated

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

-- *** Contact

data Contact
    = Contact
    { _contactName  :: !T.Text
    , _contactEmail :: !T.Text
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''Contact)

instance ToJSON Contact where
    toJSON     = genericToJSON     (googleOptions 8)
    toEncoding = genericToEncoding (googleOptions 8)

instance FromJSON Contact where
    parseJSON = genericParseJSON (googleOptions 8)

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Dime.Types where


import qualified Codec.Binary.QuotedPrintable as QP
import           Control.Applicative
import           Control.Arrow                ((&&&))
import           Control.Error
import           Control.Lens                 hiding (at, (.=))
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Free
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Control
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor               (first)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy.Char8   as L8
import           Data.Data
import           Data.Foldable                (fold)
import qualified Data.Text                    as T
import           Data.Text.Encoding
import           Data.Time
import           Data.Time.Clock.POSIX        (posixSecondsToUTCTime)
import           Database.Persist.Sql
import           Database.Persist.TH
import           GHC.Generics                 hiding (to)
import           Network.HTTP.Client.Internal hiding ((<>))
import           Network.OAuth.OAuth2
import           Network.Wreq.Types           hiding (Options, Payload)
import           Web.Twitter.Types.Lens       hiding (Entity, User)

import           Dime.Types.Fields
import           Dime.Types.Google
import           Dime.Utils


type GetParam = (T.Text, [T.Text])

-- * Types.Source

class ToPostObject o where
    toPostObject   :: o -> Maybe PostObject
    fromPostObject :: PostObject -> Maybe o
    getSource      :: o -> PostSource
    getSourceId    :: o -> T.Text
    getSender      :: o -> Maybe T.Text
    getMessage     :: o -> Maybe T.Text
    getMetadata    :: o -> [GetParam]
    getSent        :: o -> Maybe UTCTime

    toPostObject   = const Nothing
    fromPostObject = const Nothing
    getSource      = const Gmail
    getSender      = const Nothing
    getMessage     = const Nothing
    getMetadata    = const []
    getSent        = const Nothing

-- * Types.Fields

instance ToPostObject Label where
    getSourceId = _labelId

instance ToPostObject Labels where
    getSourceId = const T.empty

instance ToPostObject MessageList where
    getSourceId = const T.empty

instance ToPostObject Thread where
    getSourceId = _threadId

instance ToPostObject ThreadList where
    getSourceId = const T.empty

instance ToPostObject Message where
    toPostObject = Just . PostGmail
    fromPostObject (PostGmail   m) = Just m
    fromPostObject (PostTwitter _) = Nothing

    getSource    = const Gmail
    getSourceId  = view messageId
    getSender    = fmap mconcat . lookup "From" . getMetadata
    getMessage m =   ( pl ^? payloadBody . attachmentData . _Just
                     . to unBytes . to decodeUtf8)
                 <|> (   hush . fmap decodeUtf8 . QP.decode . unBytes
                     =<< preview (payloadBody . attachmentData . _Just)
                     =<< listToMaybe . filter isTextPart
                     =<< pl ^. payloadParts
                     )
                 where
                     pl = m ^. messagePayload
                     isTextPart :: Payload -> Bool
                     isTextPart = any isTextHeader . fold . _payloadHeaders
                     isTextHeader :: Header -> Bool
                     isTextHeader Header{..} =  _headerName  == "Content-Type"
                                             && _headerValue == "text/plain; charset=utf-8"
    getMetadata  = fmap headerToParam
                 . fold
                 . view (messagePayload . payloadHeaders)
    getSent      = hush
                 . fmap (posixSecondsToUTCTime . realToFrac)
                 . (decimalE :: T.Text -> Either String Integer)
                 . view messageInternalDate

instance ToPostObject DirectMessage where
    toPostObject   = Just . PostTwitter
    fromPostObject (PostGmail   _) = Nothing
    fromPostObject (PostTwitter m) = Just m

    getSource      = const Twitter
    getSourceId    = T.pack . show . view dmId
    getSender      = Just . view dmSenderScreenName
    getMessage     = Just . view dmText
    getMetadata dm = [ ("From", [dm ^. dmSenderScreenName])
                     , ("To"  , [dm ^. dmRecipientScreeName])
                     , ("Date", [rfc822Date $ dm ^. dmCreatedAt])
                     , ("ID"  , [T.pack . show $ dm ^. dmId])
                     ]
    getSent        = Just . view dmCreatedAt

-- * Types

unString :: MonadPlus m => Value -> m T.Text
unString (Object _) = mzero
unString (Array  _) = mzero
unString (String s) = return s
unString (Number _) = mzero
unString (Bool   _) = mzero
unString Null       = mzero

bsKey :: Object -> T.Text -> Parser ByteString
bsKey o k = fmap encodeUtf8 . unString =<< o .: k

type UserName = T.Text

data ConsumerKey
    = CKey
        { _ckeyKey    :: !ByteString
        , _ckeySecret :: !ByteString
        }
        deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''ConsumerKey)

instance FromJSON ConsumerKey where
    parseJSON (Object v) = CKey <$> bsKey v "key" <*> bsKey v "secret"
    parseJSON _          = mzero

instance ToJSON ConsumerKey where
    toJSON (CKey k s) = object [ "key"    .= decodeUtf8 k
                               , "secret" .= decodeUtf8 s
                               ]

data TwitterToken
    = TToken
        { _ttokenToken  :: !ByteString
        , _ttokenSecret :: !ByteString
        }
        deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''TwitterToken)

instance FromJSON TwitterToken where
    parseJSON (Object v) = TToken <$> bsKey v "token" <*> bsKey v "secret"
    parseJSON _          = mzero

instance ToJSON TwitterToken where
    toJSON (TToken t s) = object [ "token"  .= decodeUtf8 t
                                 , "secret" .= decodeUtf8 s
                                 ]

data TwitterLoginInfo
    = TwitterLoginInfo
    { _loginKey   :: !ConsumerKey
    , _loginToken :: !(Maybe TwitterToken)
    }
    deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''TwitterLoginInfo)

instance FromJSON TwitterLoginInfo where
    parseJSON (Object v) = TwitterLoginInfo <$> v .: "key" <*> v .:? "token"
    parseJSON _          = mzero

instance ToJSON TwitterLoginInfo where
    toJSON (TwitterLoginInfo k t) = object [ "key"   .= k
                                           , "token" .= t
                                           ]

data LoginInfo
    = LoginInfo
    { _loginTwitter :: !(Maybe TwitterLoginInfo)
    , _loginGmail   :: !(Maybe ByteString)
    }
$(makeLenses ''LoginInfo)

instance FromJSON LoginInfo where
    parseJSON (Object v) =   LoginInfo
                         <$> v .:? "twitter"
                         <*> (fmap encodeUtf8 <$> v .:? "gmail")
    parseJSON _          = mzero

instance ToJSON LoginInfo where
    toJSON (LoginInfo t g) = object [ "twitter" .= t
                                    , "gmail"   .= fmap decodeUtf8 g
                                    ]

data IdCursor
    = NotStarted
    | Cursor Integer
    | CursorDone
  deriving (Eq, Show, Data, Typeable, Generic)

cursorDoneMaybe :: IdCursor -> Maybe Integer
cursorDoneMaybe (Cursor i) = Just i
cursorDoneMaybe _          = Nothing

isCursorDone :: IdCursor -> Bool
isCursorDone CursorDone = True
isCursorDone _          = False

share [mkPersist (sqlSettings { mpsGenerateLenses = True }), mkMigrate "migrateAll"]
    [persistLowerCase|
ArchiveSession json
    source PostSource
    created UTCTime default=CURRENT_TIME
    deriving Show Eq

Post json
    source PostSource
    sourceId T.Text
    sender T.Text
    message T.Text
    metadata (JSONField [GetParam])
    raw (JSONField PostObject)
    sent UTCTime Maybe
    created UTCTime default=CURRENT_TIME
    sessionId ArchiveSessionId
    UniquePostSourceId sourceId
    deriving Show Eq

DownloadCache json
    url String
    params (JSONField [GetParam])
    source PostSource
    postId PostId Maybe
    created UTCTime default=CURRENT_TIME
    started UTCTime Maybe
    done UTCTime Maybe
    UniqueDownloadUrl url params
    deriving Show Eq
|]

-- * Google.Types.DSL

-- * Google DSL

-- TODO: Can I add a phantom type that will allow me to say when batch actions
-- aren't appropriate? How would combining these work?
-- TODO: Or even better, to trace dependencies between calls automatically and
-- use that to know when actions can be batched?
data GoogleActionF a where
    GGet  :: (FromJSON response, ToPostObject response)
          => URI -> [GetParam]         -> (response -> a) -> GoogleActionF a
    GPost :: (FromJSON response, Postable post)
          => URI -> [GetParam] -> post -> (response -> a) -> GoogleActionF a

instance Functor GoogleActionF where
    fmap f (GGet  uri ps   r) = GGet  uri ps   $ f . r
    fmap f (GPost uri ps p r) = GPost uri ps p $ f . r

type GoogleAction n = Free GoogleActionF n

-- * Google.Types.Monad

-- * Google monad

data DimeData
    = DimeData
    { _ddManager     :: !Manager
    , _ddAccessToken :: !AccessToken
    , _ddSqlBackend  :: !SqlBackend
    } deriving (Typeable, Generic)
$(makeLenses ''DimeData)

data DimeState
    = DimeState
    { _dsSession :: !(Maybe (Entity ArchiveSession))
    , _dsSource  :: !PostSource
    } deriving (Typeable, Generic)
$(makeLenses ''DimeState)

newtype DimeT m a
    = DimeT { unDimeT :: RWST DimeData () DimeState
                              (LoggingT (ExceptT String m))
                              a }
    deriving ( Functor, Applicative, Monad, MonadReader DimeData
             , MonadState DimeState, MonadIO, MonadLogger, Generic
             )

type Dime = DimeT IO

instance MonadTrans DimeT where
    lift = DimeT . lift . lift . lift

instance MonadThrow m => MonadThrow (DimeT m) where
    throwM = DimeT . lift . lift . throwE . show

instance MonadTransControl DimeT where
    type StT DimeT a = StT (RWST DimeData () DimeState) a
    liftWith runG = DimeT $ liftWith $ \runR ->
                            liftWith $ \runL ->
                            liftWith $ \runE ->
                                runG $ (either fail return =<<)
                                     . runE . runL . runR . unDimeT
    restoreT = DimeT . restoreT . restoreT . restoreT . fmap Right

instance MonadBase b m => MonadBase b (DimeT m) where
    liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (DimeT m) where
    type StM (DimeT m) a = ComposeSt DimeT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

runDime :: PostSource -> Manager -> AccessToken -> SqlBackend -> Dime a
        -> Script a
runDime src m t s = runStderrLoggingT . runDimeL src m t s

runDimeL :: PostSource -> Manager -> AccessToken -> SqlBackend -> Dime a
         -> LoggingT (ExceptT String IO) a
runDimeL src m t s g =
    view _1 <$> runRWST (unDimeT g) (DimeData m t s) (DimeState Nothing src)

currentManager :: Monad m => DimeT m Manager
currentManager = view ddManager

currentAccessToken :: Monad m => DimeT m AccessToken
currentAccessToken = view ddAccessToken

currentSqlBackend :: Monad m => DimeT m SqlBackend
currentSqlBackend = view ddSqlBackend

currentManagerToken :: Monad m => DimeT m (Manager, AccessToken)
currentManagerToken = asks (_ddManager &&& _ddAccessToken)

liftSql :: Monad m
        => ReaderT SqlBackend (LoggingT (ExceptT String m)) a -> DimeT m a
liftSql sql = DimeT . RWST $ \r s ->
    (, s, mempty) <$> runReaderT sql (r ^. ddSqlBackend)

liftE :: Script a -> Dime a
liftE = DimeT . lift . lift

liftG :: IO (OAuth2Result a) -> Dime a
liftG = liftE . liftSG

liftSG :: IO (OAuth2Result a) -> Script a
liftSG = ExceptT . fmap (first L8.unpack)

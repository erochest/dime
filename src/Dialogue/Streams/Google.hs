{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}


module Dialogue.Streams.Google where


import qualified Codec.Binary.QuotedPrintable as QP
import           Control.Applicative
import           Control.Error
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types             hiding (Options)
import qualified Data.Aeson.Types             as AT
import           Data.Bifunctor
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as C8
import qualified Data.ByteString.Lazy.Char8   as L8
import qualified Data.ByteString.Lazy.Char8   as BL8
import           Data.Char                    (toLower)
import           Data.Data
import           Data.Foldable
import qualified Data.HashMap.Strict          as M
import           Data.Monoid
import qualified Data.Sequence                as Seq
import qualified Data.Text                    as T
import           Data.Text.Encoding
import           Data.Text.Format
import qualified Data.Text.Lazy               as TL
import           Data.Time
import           Data.Time.Clock.POSIX
import           Database.Persist
import           GHC.Generics                 hiding (to)
import           Network.HTTP.Conduit         hiding (responseBody)
import           Network.OAuth.OAuth2
import           Network.Wreq                 hiding (Payload, header)
import           System.Environment

import           Dialogue.Fields
import           Dialogue.Handles
import           Dialogue.Models
import           Dialogue.Types.Dialogue
import           Dialogue.Utils


newtype GoogleException = GoogleException { unGoogleException :: T.Text }
                        deriving (Show, Eq, Data, Typeable, Generic)

instance Exception GoogleException

data GoogleStream
    = GoogleStream
    { _googleServiceId :: !(Maybe ServiceInfoId)
    , _googleRefresh   :: !ByteString
    } deriving (Show, Eq, Typeable, Generic)
$(makeClassy ''GoogleStream)
$(makeFields ''GoogleStream)

googleOptions :: Int -> AT.Options
googleOptions n = defaultOptions
                { fieldLabelModifier = lowerFirst . drop n
                }
    where
        lowerFirst []     = []
        lowerFirst (x:xs) = toLower x : xs

data Header
    = Header
    { _headerName  :: !T.Text
    , _headerValue :: !T.Text
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassy ''Header)

instance ToJSON Header where
    toJSON     = genericToJSON     (googleOptions 7)
    toEncoding = genericToEncoding (googleOptions 7)

instance FromJSON Header where
    parseJSON = genericParseJSON (googleOptions 7)

headerToParam :: Header -> (T.Text, [T.Text])
headerToParam (Header n v) = (n, [v])

data Attachment
    = Attachment
    { _attachmentId   :: !(Maybe T.Text)
    , _attachmentSize :: !Int
    , _attachmentData :: !(Maybe T.Text)
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassy ''Attachment)

instance ToJSON Attachment where
    toJSON     = genericToJSON     (googleOptions 11)
    toEncoding = genericToEncoding (googleOptions 11)

instance FromJSON Attachment where
    parseJSON = genericParseJSON (googleOptions 11)

data Payload
    = Payload
    { _payloadPartId   :: !(Maybe T.Text)
    , _payloadMimeType :: !T.Text
    , _payloadFilename :: !T.Text
    , _payloadHeaders  :: !(Maybe [Header])
    , _payloadBody     :: !Attachment
    , _payloadParts    :: !(Maybe [Payload])
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassy ''Payload)

instance ToJSON Payload where
    toJSON     = genericToJSON     (googleOptions 8)
    toEncoding = genericToEncoding (googleOptions 8)

instance FromJSON Payload where
    parseJSON = genericParseJSON (googleOptions 8)

data MessageShort
    = MessageShort
    { _messageShortId       :: !T.Text
    , _messageShortThreadId :: !T.Text
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassy ''MessageShort)

instance ToJSON MessageShort where
    toJSON     = genericToJSON     (googleOptions 13)
    toEncoding = genericToEncoding (googleOptions 13)

instance FromJSON MessageShort where
    parseJSON = genericParseJSON (googleOptions 13)

data Message
    = Message
    { _messageId           :: !T.Text
    , _messageThreadId     :: !T.Text
    , _messageLabelIds     :: ![T.Text]
    , _messageSnippet      :: !(Maybe T.Text)
    , _messageHistoryId    :: !T.Text
    , _messageInternalDate :: !T.Text
    , _messagePayload      :: !Payload
    , _messageSizeEstimate :: !Int
    , _messageRaw          :: !(Maybe T.Text)
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassy ''Message)

instance ToJSON Message where
    toJSON     = genericToJSON     (googleOptions 8)
    toEncoding = genericToEncoding (googleOptions 8)

instance FromJSON Message where
    parseJSON = genericParseJSON (googleOptions 8)

data MessageList
    = MessageList
    { _messagesMessages           :: ![MessageShort]
    , _messagesNextPageToken      :: !(Maybe T.Text)
    , _messagesResultSizeEstimate :: !Int
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''MessageList)

instance ToJSON MessageList where
    toJSON     = genericToJSON     (googleOptions 9)
    toEncoding = genericToEncoding (googleOptions 9)

instance FromJSON MessageList where
    parseJSON = genericParseJSON (googleOptions 9)

liftFetch :: IO (OAuth2Result a) -> Dialogue a
liftFetch =   hoistE
          .   first (toException . GoogleException . decodeUtf8 . L8.toStrict)
          <=< liftIO

loginGoogle :: Dialogue ByteString
loginGoogle = do
    oauth2  <- googleOAuth
    let url =  authorizationUrl oauth2 `appendQueryParam` googleScope
    pin     <- liftIO . runResourceT . getPIN "GMail" $ C8.unpack url
    m       <- liftIO $ newManager tlsManagerSettings
    token   <- liftFetch $ fetchAccessToken m oauth2 pin
    hoistM (GoogleException "No refresh token") $ refreshToken token
    where
        googleScopeEmail, googleScopeRead, googleScopeInsert, googleScopeLabels
            , googleScope :: QueryParams

        googleScopeEmail  = [("scope", "https://www.googleapis.com/auth/userinfo.email")]
        googleScopeRead   = [("scope", "https://www.googleapis.com/auth/gmail.readonly")]
        googleScopeInsert = [("scope", "https://www.googleapis.com/auth/gmail.insert")]
        googleScopeLabels = [("scope", "https://www.googleapis.com/auth/gmail.labels")]
        googleScope = pure
                    . ("scope",)
                    . joinScopes
                    . fmap snd
                    $ mconcat
                    [ googleScopeEmail
                    , googleScopeRead
                    , googleScopeInsert
                    , googleScopeLabels
                    ]

        joinScopes :: [B.ByteString] -> B.ByteString
        joinScopes = B.intercalate " "

googleOAuth :: Dialogue OAuth2
googleOAuth = do
    k <- fmap C8.pack
      .  hoistM' (GoogleException "Missing GOOGLE_KEY") . liftIO
      $  lookupEnv "GOOGLE_KEY"
    s <- fmap C8.pack
      .  hoistM' (GoogleException "Missing GOOGLE_SECRET") . liftIO
      $  lookupEnv "GOOGLE_SECRET"
    return $ OAuth2 k s "https://accounts.google.com/o/oauth2/auth"
                        "https://accounts.google.com/o/oauth2/token"
                        (Just "urn:ietf:wg:oauth:2.0:oob")

loadGoogle :: Dialogue GoogleStream
loadGoogle = do
    s <- liftSql
      $  selectFirst [ ServiceInfoService      ==. GoogleService
                     , ServiceInfoIsActive     ==. True
                     , ServiceInfoAccessToken  !=. Nothing
                     , ServiceInfoAccessSecret ==. Nothing
                     ] []
    maybe (throwD ex) return $ do
        (Entity sid s') <- s
        token <- encodeUtf8 <$> _serviceInfoAccessToken  s'
        return $ GoogleStream (Just sid) token
    where
        ex = GoogleException "Invalid service: Google"

saveGoogle :: GoogleStream -> Dialogue (ServiceInfoId, GoogleStream)
saveGoogle gs@(GoogleStream (Just sid) t) = do
    liftSql $ update sid [ ServiceInfoAccessToken  =. Just (decodeUtf8 t)
                         ]
    return (sid, gs)
saveGoogle gs@(GoogleStream Nothing t) = do
    k <- liftSql
      .  insert
      $  ServiceInfo GoogleService False (Just (decodeUtf8 t)) Nothing
    return (k, gs & googleServiceId ?~ k)

lastGoogleUpdateDate :: Dialogue (Maybe UTCTime)
lastGoogleUpdateDate =
    liftSql $   fmap (_googleMessageCreatedAt . entityVal)
            <$> selectFirst [] [Desc GoogleMessageCreatedAt]

lastGoogleUpdateID :: Dialogue (Maybe T.Text)
lastGoogleUpdateID =
    liftSql $   fmap (_googleMessageGoogleId . entityVal)
            <$> selectFirst [] [Desc GoogleMessageCreatedAt]

downloadGoogleMessages :: GoogleStream -> Dialogue [Entity GoogleMessage]
downloadGoogleMessages gs = do
    oauth2   <-  googleOAuth
    idx      <-  indexHandles GoogleService
    -- lastId   <-  lastGoogleUpdateID
    m        <-  liftIO $ newManager tlsManagerSettings
    token    <-  liftFetch $ fetchRefreshToken m oauth2 $ gs ^. googleRefresh
    profiles <-  liftSql $ selectList [ ProfilePrimary  ==. False ] []
    other    <-  hoistM (GoogleException "No conversant")
             .   fmap (_handleHandle . entityVal)
             =<< liftSql (selectFirst [ HandleService   ==. GoogleService
                                      , HandleProfileId <-. map entityKey profiles
                                      ] [])
    let q    = TL.toStrict $ format "{{from:{} to:{}}}" (other, other)
        opts = defaults
             & manager   .~ Right m
             & auth      ?~ oauth2Bearer (accessToken token)
        optq = opts & param "q" .~ [q]
    me       <-  hoistM (GoogleException "Invalid user profile")
             .   preview (key "emailAddress" . _String)
             =<< (asJSON' :: Response BL8.ByteString -> Dialogue Value)
             =<< liftIO (getWith opts meUrl)
    messages <- go opts optq Nothing
    liftSql
        $ fmap catMaybes
        . mapM insertUniqueEntity
        . mapMaybe (toGM idx (me, other))
        $ toList messages
    where
        url   = "https://www.googleapis.com/gmail/v1/users/me/messages"
        meUrl = "https://www.googleapis.com/gmail/v1/users/me/profile"

        go :: Options -> Options -> Maybe T.Text -> Dialogue (Seq.Seq Message)
        go o oq p = do
            let o' = maybe oq (flip (set (param "pageToken")) oq . pure) p
            ml <- asJSON' =<< liftIO (getWith o' url)
            ms <-  mapM asJSON'
               =<< mapM (liftIO . getWith o . (url <>) . ("/" <>) . T.unpack)
               (   ml ^.. messagesMessages . traverse . messageShortId)
            mappend (Seq.fromList ms)
                <$> maybe (return mempty) (go o oq . Just)
                (   ml ^. messagesNextPageToken)

            -- TODO: should i add these into database at this point or do it later?
            -- TODO: catch errors and abort?

asJSON' :: FromJSON a => Response BL8.ByteString -> Dialogue a
asJSON' = hoistE . (toException `bimap` view responseBody) . asJSON

hdr :: T.Text -> Prism' Header Header
hdr n = prism' id (\h -> if _headerName h == n then Just h else Nothing)

toGM :: HandleIndex -> (T.Text, T.Text) -> Message -> Maybe GoogleMessage
toGM idx users m = do
    let pl = m ^. messagePayload
    (s, r)  <- getUsers idx users $ pl ^.. payloadHeaders . _Just . traverse
    time    <- hush
            .  fmap (posixSecondsToUTCTime . realToFrac)
            .  (decimalE :: T.Text -> Either String Integer)
            $  m ^. messageInternalDate
    content <-  pl ^. payloadBody . attachmentData
            <|> (   hush . fmap decodeUtf8 . QP.decode
                =<< preview (payloadBody . attachmentData . _Just . to encodeUtf8)
                =<< listToMaybe . filter isTextPart
                =<< pl ^. payloadParts
                )
    return
        $ GoogleMessage (m ^. messageId) time s r content
        . decodeUtf8
        . BL8.toStrict
        $ encode m
    where
        getUsers :: HandleIndex -> (T.Text, T.Text) -> [Header]
                 -> Maybe (HandleId, HandleId)
        getUsers i (u1, u2) hs = do
            sender <- hs ^? traverse . hdr "From" . headerValue
            let recipient = if sender == u1 then u2 else u1
            (,) <$> M.lookup sender i <*> M.lookup recipient i

        isTextPart :: Payload -> Bool
        isTextPart = any isTextHeader . fold . _payloadHeaders

        isTextHeader :: Header -> Bool
        isTextHeader Header{..} =  _headerName == "Content-Type"
                                && _headerValue == "text/plain; charset=utf-8"

getGoogleMessages :: Dialogue [Entity GoogleMessage]
getGoogleMessages = liftSql $ selectList [] [Asc GoogleMessageCreatedAt]

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


import qualified Codec.Binary.Base64Url                as B64
import           Conduit
import           Control.Applicative
import           Control.Arrow                         ((&&&))
import           Control.Concurrent                    hiding (yield)
import           Control.Error
import           Control.Exception.Safe
import qualified Control.Foldl                         as L
import           Control.Lens                          hiding ((...))
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Aeson.Types                      as AT
import           Data.Bifunctor
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString                       as B
import qualified Data.ByteString.Char8                 as C8
import qualified Data.ByteString.Lazy.Char8            as L8
import           Data.Data
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict                   as M
import qualified Data.HashSet                          as S
import           Data.List.Split
import           Data.Monoid
import qualified Data.Text                             as T
import           Data.Text.Encoding
import           Data.Text.Format
-- import qualified Data.Text.Format                      as F
import qualified Data.Text.Lazy                        as TL
import           Data.Time
import           Data.Time.Clock.POSIX
import           Database.Persist
import           GHC.Generics                          hiding (to)
import           Network.HTTP.Client.Internal          hiding (responseBody,
                                                        (<>))
import           Network.HTTP.Client.MultipartFormData (renderParts,
                                                        webkitBoundary)
import           Network.HTTP.Conduit                  hiding (lbsResponse,
                                                        responseBody)
import           Network.HTTP.Types.Header             (hContentType)
import           Network.HTTP.Types.Method
import           Network.OAuth.OAuth2
import           Network.Wreq                          hiding (Payload, header)
import           Network.Wreq.Types                    hiding (Payload, auth,
                                                        manager)
import           System.Environment

import           Dialogue.Fields
import           Dialogue.Handles
import           Dialogue.Models
import           Dialogue.Types.Dialogue
import           Dialogue.Utils


messagesPath, messagesUrl, threadsPath, threadsUrl, meUrl, batchUrl :: String

messagesPath = "/gmail/v1/users/me/messages"
messagesUrl  = "https://www.googleapis.com" <> messagesPath
threadsPath  = "/gmail/v1/users/me/threads"
threadsUrl   = "https://www.googleapis.com" <> threadsPath
meUrl        = "https://www.googleapis.com/gmail/v1/users/me/profile"
batchUrl     = "https://www.googleapis.com/batch"

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
googleOptions = prefixOptions

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
    , _messageShortThreadId :: !(Maybe T.Text)
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
    , _messageThreadId     :: !(Maybe T.Text)
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

data Thread
    = Thread
    { _threadId        :: !T.Text
    , _threadSnippet   :: !(Maybe T.Text)
    , _threadHistoryId :: !T.Text
    , _threadMessages  :: !(Maybe [Message])
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassy ''Thread)

instance ToJSON Thread where
    toJSON     = genericToJSON     (googleOptions 7)
    toEncoding = genericToEncoding (googleOptions 7)

instance FromJSON Thread where
    parseJSON = genericParseJSON (googleOptions 7)

newtype MultipartMixed = MultiMixed { unMix :: [Part] }
                       deriving (Show, Typeable, Generic)
$(makeLenses ''MultipartMixed)

instance Postable MultipartMixed where
    -- copied and pasted from Network.HTTP.Client.MultipartFormData.
    -- Yuck.
    -- Brittle.
    postPayload (MultiMixed ps) req = do
        boundary <- webkitBoundary
        body <- renderParts boundary ps
        return $ req
               { method = methodPost
               , requestHeaders =
                   (hContentType , "multipart/mixed; boundary=" <> boundary)
                    : filter (\(x, _) -> x /= hContentType) (requestHeaders req)
               , requestBody = body
               }

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
    idx       <-  indexHandles GoogleService
    messages  <-  liftSql $ selectList [] []
    profiles  <-  liftSql $ selectList [ ProfilePrimary  ==. False ] []
    other     <-  hoistM (GoogleException "No conversant")
              .   fmap (_handleHandle . entityVal)
              =<< liftSql (selectFirst [ HandleService   ==. GoogleService
                                       , HandleProfileId <-. map entityKey profiles
                                       ] [])

    oauth2    <-  googleOAuth
    m         <-  liftIO $ newManager tlsManagerSettings
    token     <-  liftFetch $ fetchRefreshToken m oauth2 $ gs ^. googleRefresh

    let seen  = L.fold (setOf (_googleMessageGoogleId .  entityVal)) messages
        q     = TL.toStrict $ format "{{from:{} to:{}}}" (other, other)
        opts  = defaults
              & manager  .~ Right m
              & auth     ?~ oauth2Bearer (accessToken token)
        optq  = opts & param "q" .~ [q]
                     & param "maxResults" .~ ["100"]

    me        <-  hoistM (GoogleException "Invalid user profile")
              .   preview (key "emailAddress" . _String)
              =<< (asJSON' :: Response L8.ByteString -> Dialogue Value)
              =<< liftIO (getWith opts meUrl)

    -- TODO: catch errors and abort?
    -- TODO: remove watches and traces
    runResourceT
        $   pages optq Nothing
        =$= mapC _messageShortId
        =$= filterC (not . (`S.member` seen))
        =$= chunkC 50
        =$= iterMC      (liftIO . const (threadDelay 5000))
        =$= mapMC       ( lift
                        . batchGet opts
                        . map (id &&& ( TL.unpack
                                      . format "{}/{}"
                                      . (messagesPath,))))
        =$= concatMapC  (view responseBody)
        =$= mapMC       (lift . jsonMsg)
        =$= concatMapC  (toGM idx (me, other))
        =$= concatMapMC (lift . liftSql . insertUniqueEntity)
        $$  sinkList

    where
        {- markerC :: MonadIO m => String -> Conduit a m a -}
        {- markerC x = iterMC (const $ liftIO $ Prelude.putStrLn x) -}

        jsonMsg :: Response L8.ByteString -> Dialogue Message
        jsonMsg = asJSON'

        setOf :: (Eq b, Hashable b) => (a -> b) -> L.Fold a (S.HashSet b)
        setOf f = L.Fold (flip (S.insert . f)) S.empty id

        batchGet :: Options -> [(T.Text, String)] -> Dialogue (Response [Response L8.ByteString])
        batchGet o gets = do
            msg <- liftIO
                $  postWith o batchUrl
                $  MultiMixed
                $  map ( (partContentType ?~ "application/http")
                       . uncurry partString
                       . second (("GET " <>) . (<> "\r\n"))
                       ) gets
            -- let filename = TL.unpack $ F.format "multi-{}.txt" $ Only $ fst $ head gets
            -- liftIO $ L8.writeFile filename $ msg ^. responseBody
            parseMulti msg

        chunkC :: Monad m => Int -> Conduit a m [a]
        chunkC n = do
            xs <- catMaybes <$> replicateM n await
            case xs of
                []  -> return ()
                xs' -> yield xs' >> chunkC n

        pages :: Options -> Maybe T.Text
              -> Producer (ResourceT Dialogue) MessageShort
        pages o p = do
            let o' = maybe o (flip (set (param "pageToken")) o . pure) p
            ml <- lift . lift . asJSON' =<< liftIO (getWith o' messagesUrl)
            case ml ^. messagesMessages of
                [] -> return ()
                ms -> do
                    yieldMany ms
                    case ml ^. messagesNextPageToken of
                        pt@(Just _) -> pages o pt
                        Nothing     -> return ()

parseMulti :: Response L8.ByteString -> Dialogue (Response [Response L8.ByteString])
parseMulti r
    | not (isMultipartMixed r) = return $ const [r] <$> r
    | otherwise =
        traverse (parseMulti' (r ^. responseHeader "Content-Type")) r

parseMulti' :: C8.ByteString -> L8.ByteString -> Dialogue [Response L8.ByteString]
parseMulti' cType multi = do
    -- NB: 'drop 2' below just gets rid of a
    -- "Content-Type: application/http" header
    cxns <- liftIO
         .  mapM ( dummyConnection
                 . pure
                 . L8.toStrict
                 . L8.unlines
                 . drop 2)
         .  wordsBy (C8.isInfixOf b . L8.toStrict)
         $  L8.lines multi
    forM cxns $ \(c, _, _) -> do
        req  <- liftIO . parseRequest $ "GET " <> messagesUrl
        liftIO
            $   lbsResponse
            =<< getResponse (const $ return ()) Nothing req c Nothing
    where
        b = C8.concat
          . concatMap (drop 1 . C8.split '=')
          . drop 1
          $ C8.split ';' cType

isMultipartMixed :: Response L8.ByteString -> Bool
isMultipartMixed r =
    "multipart/mixed" `C8.isPrefixOf` (r ^. responseHeader "Content-Type")

asJSON' :: FromJSON a => Response L8.ByteString -> Dialogue a
asJSON' = hoistE . (toException `bimap` view responseBody) . asJSON

hdr :: T.Text -> Prism' Header Header
hdr n = prism' id (\h -> if _headerName h == n then Just h else Nothing)

lookupHeader :: T.Text -> Message -> Maybe T.Text
lookupHeader name =
    preview ( messagePayload
            . payloadHeaders
            . _Just
            . traverse
            . hdr name
            . headerValue
            )

decodeContent :: C8.ByteString -> Either SomeException T.Text
decodeContent = (       (toException . GoogleException . T.pack . show)
                `bimap` decodeUtf8)
              . B64.decode

decodeDate :: T.Text -> Either SomeException UTCTime
decodeDate = (       (toException . GoogleException . T.pack)
             `bimap` (posixSecondsToUTCTime . (/1000) . realToFrac))
           . (decimalE :: T.Text -> Either String Integer)

toGM :: HandleIndex -> (T.Text, T.Text) -> Message -> Either SomeException GoogleMessage
toGM idx users m = do
    let pl = m ^. messagePayload
    (s, r)  <-  note (toException $ GoogleException "No users.")
            $   getUsers idx users
            $   pl ^.. payloadHeaders . _Just . traverse
    time    <-  decodeDate $ m ^. messageInternalDate
    content <-  note (toException $ GoogleException "No payload.")
            $    pl ^? payloadBody . attachmentData . _Just . to encodeUtf8
            <|> (   preview (payloadBody . attachmentData . _Just . to encodeUtf8)
                =<< listToMaybe . filter isTextPart
                =<< pl ^. payloadParts
                )
    decoded <-  decodeContent content
    return
        $ GoogleMessage (m ^. messageId) (m ^. messageThreadId)
                        time s r decoded
        . decodeUtf8
        . L8.toStrict
        $ encode m
    where
        getUsers :: HandleIndex -> (T.Text, T.Text) -> [Header]
                 -> Maybe (HandleId, HandleId)
        getUsers i (u1, u2) hs = do
            sender <- hs ^? traverse
                         .  hdr "From" . headerValue
                         .  to (fold . hush . parseEmail) . traverse . _2
            let recipient = if sender == u1 then u2 else u1
            (,) <$> M.lookup sender i <*> M.lookup recipient i

        isTextPart :: Payload -> Bool
        isTextPart = any isTextHeader . fold . _payloadHeaders

        isTextHeader :: Header -> Bool
        isTextHeader Header{..} =
            _headerName  == "Content-Type"
                && "text/plain" `T.isPrefixOf` _headerValue

getGoogleMessages :: Dialogue [Entity GoogleMessage]
getGoogleMessages = liftSql $ selectList [] [Asc GoogleMessageCreatedAt]

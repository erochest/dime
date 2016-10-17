{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}


module Dialogue.Streams.GDoc where


import           Conduit
import           Control.Concurrent         (threadDelay)
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Data
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Data.Text.Format
import qualified Data.Text.Lazy             as TL
import           Data.Time
import           Database.Persist
import           GHC.Generics
import           Network.HTTP.Conduit       hiding (responseBody)
import           Network.OAuth.OAuth2
import           Network.Wreq

import           Dialogue.Fields
import           Dialogue.Models
import           Dialogue.Streams.Google    (GoogleException (..), apiHost,
                                             asJSON', googleOAuth, liftFetch,
                                             pages)
import           Dialogue.Types.Dialogue
import           Dialogue.Utils


docPath, docUrl :: String

docPath = "/drive/v3/files"
docUrl  = apiHost <> docPath

data GDocStream
    = GDocStream
    { _gdocServiceId :: !(Maybe ServiceInfoId)
    , _gdocFolder    :: !T.Text
    , _gdocRefresh   :: !ByteString
    } deriving (Show, Eq, Typeable, Generic)
$(makeClassy ''GDocStream)
$(makeFields ''GDocStream)

loadGDoc :: Dialogue GDocStream
loadGDoc = do
    s <- liftSql
      $  selectFirst [ ServiceInfoService      ==. GDocService
                     , ServiceInfoIsActive     ==. True
                     , ServiceInfoAccessToken  !=. Nothing
                     , ServiceInfoAccessSecret !=. Nothing
                     ] []
    maybe (throwD ex) return $ do
        (Entity sid s') <- s
        GDocStream (Just sid)
            <$> _serviceInfoAccessSecret s'
            <*> fmap encodeUtf8 (_serviceInfoAccessToken  s')
    where
        ex = GoogleException "Invalid service: GDoc"

saveGDoc :: GDocStream -> Dialogue (ServiceInfoId, GDocStream)
saveGDoc gs@(GDocStream (Just sid) f t) = do
    liftSql $ update sid [ ServiceInfoAccessToken  =. Just (decodeUtf8 t)
                         , ServiceInfoAccessSecret =. Just f
                         ]
    return (sid, gs)
saveGDoc gs@(GDocStream Nothing f t) = do
    k <- liftSql
      .  insert
      $  ServiceInfo GDocService False (Just (decodeUtf8 t)) (Just f)
    return (k, gs & gdocServiceId ?~ k)

lastGDocUpdatedDate :: Dialogue (Maybe UTCTime)
lastGDocUpdatedDate =
    liftSql $   fmap (_gDocCreatedAt . entityVal)
            <$> selectFirst [] [Desc GDocCreatedAt]

lastGDocUpdatedID :: Dialogue (Maybe T.Text)
lastGDocUpdatedID =
    liftSql $   fmap (_gDocGdocId . entityVal)
            <$> selectFirst [] [Desc GDocCreatedAt]

data File
    = File
    { _fileId          :: !T.Text
    , _fileName        :: !T.Text
    , _fileMimeType    :: !T.Text
    , _fileCreatedTime :: !(Maybe UTCTime)
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassy ''File)

instance ToJSON File where
    toJSON     = genericToJSON     $ prefixOptions 5
    toEncoding = genericToEncoding $ prefixOptions 5

instance FromJSON File where
    parseJSON = genericParseJSON $ prefixOptions 5

data FileList
    = FileList
    { _fileListNextPageToken :: !(Maybe T.Text)
    , _fileListFiles         :: ![File]
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''FileList)

instance ToJSON FileList where
    toJSON     = genericToJSON     $ prefixOptions 9
    toEncoding = genericToEncoding $ prefixOptions 9

instance FromJSON FileList where
    parseJSON = genericParseJSON $ prefixOptions 9

downloadGDocs :: GDocStream -> Dialogue [Entity GDoc]
downloadGDocs gs = do
    lastUpdated <- lastGDocUpdatedDate
    author <-  fmap entityKey . hoistM (GoogleException "Missing author.")
           =<< liftSql (selectFirst [ProfilePrimary ==. True] [])
    oauth2 <- googleOAuth
    m <- liftIO $ newManager tlsManagerSettings
    token <- liftFetch $ fetchRefreshToken m oauth2 $ gs ^. gdocRefresh
    let opts = defaults & manager .~ Right m
                        & auth ?~ oauth2Bearer (accessToken token)
        fq   = TL.toStrict
             $ format "name='{}'" $ Only $ gs ^. gdocFolder
        optf = opts & param "q" .~ [fq] & param "maxResults" .~ ["1"]
    folder <-  hoistM (GoogleException "Invalid folder.")
           .   listToMaybe
           .   map _fileId
           .   _fileListFiles
           =<< (asJSON' :: Response L8.ByteString -> Dialogue FileList)
           =<< liftIO (getWith optf docUrl)
    let xq   = TL.toStrict
             $ format "'{}' in parents and\
                      \ mimeType='application/vnd.google-apps.document'{}"
             ( folder
             , foldMap ( format " and modifiedTime > '{}'"
                       . Only
                       . formatTime defaultTimeLocale
                                    (iso8601DateFormat (Just "%H:%M:%S"))
                       ) lastUpdated
             )
        optg = opts & param "q" .~ [xq]
    runResourceT
        $   pages fileListNextPageToken fileListFiles optg docUrl
        =$= mapC _fileId
        =$= iterMC      (liftIO . const (threadDelay 500))
        =$= mapMC       (lift   . exportDoc opts author)
        =$= concatMapMC (lift   . liftSql . insertUniqueEntity)
        $$  sinkList

exportDoc :: Options -> ProfileId -> T.Text -> Dialogue GDoc
exportDoc opts author fId = do
    meta      <-  (asJSON' :: Response L8.ByteString -> Dialogue File)
              =<< liftIO (getWith optm url)
    createdAt <- hoistM (GoogleException "No gdoc timestamp.")
              $  meta ^. fileCreatedTime
    contents  <-  decodeUtf8 . L8.toStrict . view responseBody
              <$> liftIO (getWith optx $ url ++ "/export")
    return $ GDoc fId createdAt author (meta ^. fileName) contents
    where
        url  = TL.unpack $ format "{}/{}" (docUrl, fId)
        optm = opts & param "fields"   .~ ["id,createdTime,name,mimeType"]
        optx = opts & param "mimeType" .~ ["text/plain"]

getGDocs :: Dialogue [Entity GDoc]
getGDocs = liftSql $ selectList [] [Asc GDocCreatedAt]

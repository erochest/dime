{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE ViewPatterns           #-}


module Dialogue.Streams where


import           Control.Lens
import           Data.ByteString          (ByteString)
import           Data.Proxy
import qualified Data.Text                as T
import           Data.Time
import           Database.Persist

import           Dialogue.Fields
import           Dialogue.Models
import           Dialogue.Streams.Adium
import           Dialogue.Streams.Google
import           Dialogue.Streams.Mail
import           Dialogue.Streams.Note
import           Dialogue.Streams.Twitter
import           Dialogue.Types.Dialogue


-- * MessageStream

-- TODO: It would be nice to relate the instance of Service that is associated
-- with each 'a' below. (I.e., use DataKinds to say that TwitterService is the
-- Service instance associated with TwitterStream.)
class MessageStream a item | a -> item where
    streamName :: a -> T.Text
    streamName' :: Proxy a -> T.Text
    streamService :: Proxy a -> Service
    getStreamFromService :: Service -> Dialogue (Maybe a)
    getStream :: Proxy a -> Dialogue (Maybe a)

    isActive :: a -> Dialogue Bool
    isActive' :: Proxy a -> Dialogue Bool
    activate :: HasServiceId a => a -> Dialogue a
    deactivate :: HasServiceId a => a -> Dialogue a

    openStream :: Dialogue a
    saveStream :: a -> Dialogue (Key ServiceInfo, a)

    getLastUpdatedDate :: a -> Dialogue (Maybe UTCTime)
    getLastUpdatedID :: a -> Dialogue (Maybe T.Text)

    downloadMessages :: a -> Dialogue [Entity item]
    retrieveMessages :: a -> Dialogue [Entity item]

    migrateMessages :: a -> Maybe (ByteString -> Dialogue ())
    insertMessage :: a -> Maybe (Dialogue ())

    isActive'       = serviceIsActive . streamService
    activate ms     = setStreamActive ms True
    deactivate ms   = setStreamActive ms False
    getStream       = getStreamFromService . streamService
    migrateMessages = const Nothing
    insertMessage   = const Nothing

instance MessageStream AdiumStream AdiumMessage where
    streamName    = const "Adium"
    streamName'   = const "Adium"
    streamService = const AdiumService
    getStreamFromService AdiumService = Just <$> openStream
    getStreamFromService _            = return Nothing

    isActive = const $ serviceIsActive AdiumService

    openStream = loadAdium
    saveStream = saveAdium

    getLastUpdatedDate = const lastAdiumUpdateDate
    getLastUpdatedID   = const lastAdiumUpdateID

    downloadMessages = getNewAdiumMessages
    retrieveMessages = const loadAdiumMessages

instance MessageStream GoogleStream GoogleMessage where
    streamName    = const "Google"
    streamName'   = const "Google"
    streamService = const GoogleService
    getStreamFromService GoogleService = Just <$> openStream
    getStreamFromService _             = pure Nothing

    isActive = const $ serviceIsActive GoogleService

    openStream = loadGoogle
    saveStream = saveGoogle

    getLastUpdatedDate = const lastGoogleUpdateDate
    getLastUpdatedID   = const lastGoogleUpdateID

    downloadMessages = downloadGoogleMessages
    retrieveMessages = const getGoogleMessages

instance MessageStream MailStream MailMessage where
    streamName           = const "Mail"
    streamName'          = const "mMail"
    streamService        = const MailService
    getStreamFromService MailService = Just <$> openStream
    getStreamFromService _           = return Nothing

    isActive = const $ serviceIsActive MailService

    openStream = loadMail
    saveStream = saveMail

    getLastUpdatedDate = const $ return Nothing
    getLastUpdatedID   = const $ return Nothing

    downloadMessages = loadMbox
    retrieveMessages = const loadMailMessages

instance MessageStream NoteStream NoteMessage where
    streamName  = const "Note"
    streamName' = const "Note"
    streamService = const NoteService
    getStreamFromService NoteService = Just <$> openStream
    getStreamFromService _           = return Nothing

    isActive = const $ serviceIsActive NoteService

    openStream = loadNote
    saveStream = saveNote

    getLastUpdatedDate = const lastNoteUpdateDate
    getLastUpdatedID   = const lastNoteUpdateID

    downloadMessages = loadNoteDirectory
    retrieveMessages = const loadNoteMessages

instance MessageStream TwitterStream TwitterMessage where
    streamName    = const "Twitter"
    streamName'   = const "Twitter"
    streamService = const TwitterService

    getStreamFromService TwitterService = Just <$> openStream
    getStreamFromService _ = return Nothing

    isActive = const (serviceIsActive TwitterService)

    openStream = loadTwitter
    saveStream = saveTwitter
    getLastUpdatedDate = const lastTwitterUpdateDate
    getLastUpdatedID   = const $ fmap (T.pack . show) <$> lastTwitterUpdateID
    downloadMessages = downloadTwitterMessages
    retrieveMessages = const getTwitterMessages


class HasServiceId a where
    serviceId :: Lens' a (Maybe ServiceInfoId)

instance HasServiceId AdiumStream where
    serviceId = adiumServiceId

instance HasServiceId GoogleStream where
    serviceId = googleServiceId

instance HasServiceId MailStream where
    serviceId = mailServiceId

instance HasServiceId NoteStream where
    serviceId = noteServiceId

instance HasServiceId TwitterStream where
    serviceId = twitterServiceId

serviceIsActive :: Service -> Dialogue Bool
serviceIsActive s =
    maybe False (_serviceInfoIsActive . entityVal)
        <$> liftSql (selectFirst [ServiceInfoService ==. s] [])

setStreamActive :: (MessageStream ms mi, HasServiceId ms)
                => ms -> Bool -> Dialogue ms
setStreamActive ms@(view serviceId -> Just sid) a = do
    liftSql $ update sid [ServiceInfoIsActive =. a]
    return ms
setStreamActive ms a = (`setStreamActive` a) =<< snd <$> saveStream ms

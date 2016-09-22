{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE ViewPatterns           #-}


module Dialogue.Streams where


import           Control.Lens
import           Control.Monad            (void)
import           Data.ByteString          (ByteString)
import           Data.Proxy
import qualified Data.Text                as T
import           Data.Time
import           Database.Persist

import           Dialogue.Fields
import           Dialogue.Models
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
    migrateMessages  = const $ Just $ void . migrateDirectMessages

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

class HasServiceId a where
    serviceId :: Lens' a (Maybe ServiceInfoId)

instance HasServiceId TwitterStream where
    serviceId = twitterServiceId

instance HasServiceId NoteStream where
    serviceId = noteServiceId

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

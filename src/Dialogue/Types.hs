{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Dialogue.Types where


import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           Data.ByteString          (ByteString)
import           Data.Data
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Data.Time
import           Database.Persist.Sqlite
import           GHC.Generics             hiding (to)
import           Lucid                    (Html)
import           System.IO                (hFlush, stdout)

import           Dialogue.Fields
import           Dialogue.Models
import           Dialogue.Streams.Twitter
import           Dialogue.Types.Dialogue


-- * Exceptions

newtype ProfileException = ProfileException { unProfileException :: T.Text }
                         deriving (Show, Eq, Data, Typeable, Generic)

instance Exception ProfileException

newtype ServiceException = ServiceException { unServiceException :: T.Text }
                         deriving (Show, Eq, Data, Typeable, Generic)

instance Exception ServiceException

-- * Streams

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
    activate :: a -> Dialogue a
    deactivate :: a -> Dialogue a

    openStream :: Dialogue a
    saveStream :: a -> Dialogue (Key ServiceInfo, a)

    getLastUpdatedDate :: a -> Dialogue (Maybe UTCTime)
    getLastUpdatedID :: a -> Dialogue (Maybe T.Text)
    updateStream :: a -> T.Text -> Dialogue a

    getRecentMessages :: a -> Dialogue [Entity item]
    retrieveMessages :: a -> Dialogue [Entity item]

    migrateMessages :: a -> Maybe (ByteString -> Dialogue ())
    insertMessage :: a -> Maybe (Dialogue ())

    getStream       = getStreamFromService . streamService
    migrateMessages = const Nothing
    insertMessage   = const Nothing

instance MessageStream TwitterStream TwitterMessage where
    streamName    = const "Twitter"
    streamName'   = const "Twitter"
    streamService = const TwitterService

    -- getStreamFromService :: Service -> Dialogue e (Maybe a)
    getStreamFromService TwitterService = Just <$> openStream
    getStreamFromService _ = return Nothing

    isActive   _  = twitterIsActive
    isActive'  _  = twitterIsActive
    activate   ts = setTwitterActive ts True
    deactivate ts = setTwitterActive ts False

    -- openStream :: Dialogue e a
    openStream = loadTwitter

    -- closeStream :: a -> Dialogue e ServiceInfoId
    saveStream = saveTwitter

    -- getLastUpdatedDate :: a -> Dialogue e UTCTime
    getLastUpdatedDate = lastTwitterUpdateDate

    -- getLastUpdatedID :: a -> Dialogue e T.Text
    getLastUpdatedID = lastTwitterUpdateID

    -- updateStream :: a -> T.Text -> Dialogue a
    updateStream ts uid = setTwitterUpdate ts uid Nothing

    -- getRecentMessages :: a -> Dialogue e [b]
    getRecentMessages = getRecentTwitterMessages

    -- retrieveMessages :: Traversable t => a -> Dialogue e (t b)
    retrieveMessages _ = getTwitterMessages

    -- migrateMessages :: a -> Maybe (ByteString -> Dialogue e ())
    migrateMessages _ = Just $ void . migrateDirectMessages

class Publishable b where
    toHTML :: b -> Html ()

data StreamList where
    MkStreamList :: MessageStream ms o => [ms] -> StreamList

data ProxyStreamList where
    MkProxyList :: MessageStream ms o => [Proxy ms] -> ProxyStreamList

-- * Services

data ExportFormat = ExportJSON | ExportEPUB
                  deriving (Eq, Show, Data, Typeable, Generic)

-- * Inputs

data TextInput
    = FileInput !FilePath
    | RawInput !T.Text
    | StdInput
    deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassyPrisms ''TextInput)

-- * Promptable

class Promptable p where
    prompt :: T.Text -> Dialogue p
    promptMaybe :: T.Text -> Dialogue (Maybe p)

    promptMaybe = fmap Just . prompt

checkPrompt :: Promptable p => T.Text -> Dialogue (Maybe p)
checkPrompt msg = do
    add <- prompt msg
    if add then Just <$> prompt "" else return Nothing

instance Promptable T.Text where
    prompt msg =  liftIO
               $  TIO.putStr (msg <> "? ")
               >> hFlush stdout
               >> T.strip <$> TIO.getLine

    promptMaybe = checkPrompt

instance Promptable Bool where
    prompt msg = liftIO $ do
        TIO.putStr (msg <> " [Y/n]? ") >> hFlush stdout
        reply <- T.toLower . T.strip <$> TIO.getLine
        return $ case T.uncons reply of
                    Nothing       -> True
                    Just ('y', _) -> True
                    Just _        -> False

instance Promptable Profile where
    prompt _msg =
        Profile <$> prompt "name" <*> prompt "nickname" <*> prompt "primary"

    promptMaybe = checkPrompt

instance Promptable TwitterStream where
    prompt msg = do
        liftIO $ TIO.putStrLn msg
        (token, secret) <- loginTwitter

        profiles <- liftSql $ selectList [] []
        forM_ profiles $ \(Entity pId p) -> do
            name' <- promptMaybe $ "Handle for " <> (p ^. profileNickname)
            case name' of
                Just name -> liftSql . insert_ $ Handle name TwitterService pId
                Nothing -> return ()

        return $ TwitterStream Nothing (token, secret)

    promptMaybe = checkPrompt

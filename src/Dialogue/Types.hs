{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}


module Dialogue.Types where


import           Control.Error
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad.Base
import qualified Control.Monad.Catch         as Catch
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import           Data.ByteString             (ByteString)
import           Data.Data
import qualified Data.Text                   as T
import           Data.Time
import           Database.Persist.Sqlite
import           GHC.Generics                hiding (to)
import           Lucid                       (Html)

import           Dialogue.Models


-- * Application Monad

data DialogueData
    = DialogueData
    { _ddSqlBackend :: !SqlBackend
    } deriving (Typeable, Generic)
$(makeClassy ''DialogueData)

data DialogueState = DialogueState

newtype DialogueT e m a
    = DialogueT { unSay :: ExceptT e
                           (RWST DialogueData () DialogueState
                           (LoggingT m))
                           a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadLogger
             , MonadReader DialogueData, MonadState DialogueState
             , Typeable, Generic
             )

type Dialogue e = DialogueT e IO

instance MonadTrans (DialogueT e) where
    lift = DialogueT . lift . lift . lift

instance (MonadThrow m, Exception e) => MonadThrow (DialogueT e m) where
    throwM = DialogueT . Catch.throwM

instance Exception e => MonadTransControl (DialogueT e) where
    type StT (DialogueT e) a = StT (RWST DialogueData () DialogueState) a

    liftWith runD =
        DialogueT $ liftWith $ \runE ->
                    liftWith $ \runR ->
                    liftWith $ \runL ->
                        runD $ (liftError =<<)
                             . runL . runR . runE . unSay
        where
            liftError :: (Exception e, Monad n) => (Either e b, c, d) -> n (b, c, d)
            liftError (Right a, b, c) = return (a, b, c)
            liftError (Left e , _, _) = fail $ displayException e

    restoreT = DialogueT . restoreT . restoreT . restoreT . fmap (over _1 Right)

instance MonadBase b m => MonadBase b (DialogueT e m) where
    liftBase = liftBaseDefault

instance (Exception e, MonadBaseControl b m)
         => MonadBaseControl b (DialogueT e m) where
    type StM (DialogueT e m) a = ComposeSt (DialogueT e) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

liftSql :: Monad m => SqlPersistT (DialogueT e m) a -> DialogueT e m a
liftSql sql = runReaderT sql =<< view ddSqlBackend

liftE :: Monad m => ExceptT e m a -> DialogueT e m a
liftE = DialogueT . ExceptT . lift . lift . runExceptT

runDialogueDB' :: Monad m
               => DialogueT e m a -> SqlBackend -> LoggingT m (Either e a)
runDialogueDB' d sqlb =
        fst <$> evalRWST (runExceptT (unSay d)) (DialogueData sqlb) DialogueState

runDialogueDB :: (MonadBaseControl IO m, MonadIO m)
              => T.Text -> DialogueT e m a -> LoggingT m (Either e a)
runDialogueDB sqlFile d =
    withSqliteConn sqlFile $
        runDialogueDB' (liftSql (runMigration migrateAll) >> d)

runDialogue :: Exception e => T.Text -> Dialogue e a -> IO a
runDialogue sqliteFile d =
    either (fail . displayException) return
        =<< runStderrLoggingT (runDialogueDB sqliteFile d)

runDialogueT :: (MonadBaseControl IO m, MonadIO m)
             => T.Text -> DialogueT e m a -> m (Either e a)
runDialogueT sqliteFile d =
    runStderrLoggingT $ runDialogueDB sqliteFile d

runDialogueL :: (Exception e, MonadBaseControl IO m, MonadIO m)
             => T.Text -> DialogueT e m a -> LoggingT (ExceptT e m) a
runDialogueL sqliteFile d =
    LoggingT $ ExceptT . runLoggingT (runDialogueDB sqliteFile d)

-- * MessageStream

class MessageStream a b where
    streamName :: a b -> T.Text
    initStream :: Dialogue e (a b)
    openStream :: Dialogue e (a b)
    closeStream :: a b -> Dialogue e ()
    getLastUpdatedDate :: a b -> Dialogue e UTCTime
    getLastUpdatedID :: a b -> Dialogue e T.Text
    getRecentMessages :: Traversable t => a b -> Dialogue e (t b)
    retrieveMessages :: Traversable t => a b -> Dialogue e (t b)
    migrateMessages :: a b -> Maybe (ByteString -> Dialogue e ())
    insertMessage :: a b -> Maybe (Dialogue e ())

    migrateMessages = const Nothing
    insertMessage   = const Nothing

class Publishable b where
    toHTML :: b -> Html ()

-- * Services

data Service = Twitter
             -- | Google
             -- | IRC
             -- | DoNote
             -- | Journal
             deriving (Eq, Read, Show, Data, Typeable, Generic)

data ExportFormat = ExportJSON | ExportEPUB
                  deriving (Eq, Show, Data, Typeable, Generic)

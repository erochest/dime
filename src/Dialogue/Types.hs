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
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}


module Dialogue.Types where


import           Control.Error
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import           Data.Bifunctor              (first)
import           Data.ByteString             (ByteString)
import           Data.Data
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Data.Time
import           Database.Persist.Sqlite
import           GHC.Generics                hiding (to)
import           Lucid                       (Html)
import           System.IO                   (hFlush, stdout)

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

liftSql :: Monad m => SqlPersistT (DialogueT e m) a -> DialogueT e m a
liftSql sql = runReaderT sql =<< view ddSqlBackend

liftE :: Monad m => ExceptT e m a -> DialogueT e m a
liftE = DialogueT . ExceptT . lift . lift . runExceptT

hoistE :: (Exception e, Monad m) => Either e a -> DialogueT e m a
hoistE (Right x) = return x
hoistE (Left  x) = liftE $ throwE x

instance MonadTrans (DialogueT e) where
    lift = DialogueT . lift . lift . lift

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

runDialogueS :: Exception e => T.Text -> Dialogue e a -> ExceptT String IO a
runDialogueS sqliteFile = ExceptT
                        . fmap (first displayException)
                        . runStderrLoggingT
                        . runDialogueDB sqliteFile

runDialogueS' :: T.Text -> Dialogue SomeException a -> Script a
runDialogueS' = runDialogueS

-- * Exceptions

newtype ProfileException = ProfileException { unProfileException :: T.Text }
                         deriving (Show, Eq, Data, Typeable, Generic)

instance Exception ProfileException

-- * MessageStream

class MessageStream a where
    data El a :: *
    streamName :: a -> T.Text
    streamName' :: Proxy a -> T.Text

    isActive :: a -> Bool
    activate :: a -> Dialogue e ()
    deactivate :: a -> Dialogue e ()

    -- emptyStream :: a b
    initStream :: Proxy a -> Dialogue e (Maybe a)
    openStream :: Dialogue e a
    closeStream :: a -> Dialogue e ()

    getLastUpdatedDate :: a -> Dialogue e UTCTime
    getLastUpdatedID :: a -> Dialogue e T.Text

    getRecentMessages :: Traversable t => a -> Dialogue e (t b)
    retrieveMessages :: Traversable t => a -> Dialogue e (t b)

    migrateMessages :: a -> Maybe (ByteString -> Dialogue e ())
    insertMessage :: a -> Maybe (Dialogue e ())

    migrateMessages = const Nothing
    insertMessage   = const Nothing

class Publishable b where
    toHTML :: b -> Html ()

data StreamList where
    MkStreamList :: MessageStream ms => [ms] -> StreamList

data ProxyStreamList where
    MkProxyList :: MessageStream ms => [Proxy ms] -> ProxyStreamList

-- * Services

data Service = Twitter
             -- | Google
             -- | IRC
             -- | DoNote
             -- | Journal
             deriving (Eq, Read, Show, Data, Typeable, Generic)

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
    prompt :: Exception e => T.Text -> Dialogue e p
    promptMaybe :: Exception e => T.Text -> Dialogue e (Maybe p)

    promptMaybe = fmap Just . prompt

instance Promptable T.Text where
    prompt msg =  liftIO
               $  TIO.putStr (msg <> "? ")
               >> hFlush stdout
               >> T.strip <$> TIO.getLine

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

    promptMaybe msg = do
        add <- prompt msg
        if add then Just <$> prompt "" else return Nothing

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}


module Dialogue.Types.Dialogue where


import           Control.Error
import           Control.Exception.Safe       hiding (MonadThrow)
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource (MonadThrow (..))
import           Data.Bifunctor               (first)
import qualified Data.Text                    as T
import           Database.Persist.Sqlite
import           GHC.Generics                 hiding (to)

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

type Dialogue = DialogueT SomeException IO

liftSql :: Monad m => SqlPersistT (DialogueT e m) a -> DialogueT e m a
liftSql sql = runReaderT sql =<< view ddSqlBackend

liftE :: Monad m => ExceptT e m a -> DialogueT e m a
liftE = DialogueT . ExceptT . lift . lift . runExceptT

hoistE :: (Exception e, Monad m) => Either e a -> DialogueT e m a
hoistE (Right x) = return x
hoistE (Left  x) = liftE $ throwE x

throwD :: (Exception e, Monad m) => e -> DialogueT SomeException m a
throwD = liftE . throwE . toException

hoistM :: (Exception e, Monad m) => e -> Maybe a -> DialogueT SomeException m a
hoistM _ (Just a) = return a
hoistM e Nothing  = throwD e

hoistM' :: (Exception e, Monad m)
        => e -> DialogueT SomeException m (Maybe a)
        -> DialogueT SomeException m a
hoistM' e a = a >>= hoistM e

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

instance Monad m => MonadThrow (DialogueT SomeException m) where
    throwM = throwD

runDialogueDB' :: Monad m
               => DialogueT e m a -> SqlBackend -> LoggingT m (Either e a)
runDialogueDB' d sqlb =
        fst <$> evalRWST (runExceptT (unSay d)) (DialogueData sqlb) DialogueState

runDialogueDB :: (MonadBaseControl IO m, MonadIO m)
              => T.Text -> DialogueT e m a -> LoggingT m (Either e a)
runDialogueDB sqlFile d =
    withSqliteConn sqlFile $
        runDialogueDB' (liftSql (runMigration migrateAll) >> d)

runDialogue :: T.Text -> Dialogue a -> IO a
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

runDialogueS :: T.Text -> Dialogue a -> ExceptT String IO a
runDialogueS sqliteFile = ExceptT
                        . fmap (first displayException)
                        . runStderrLoggingT
                        . runDialogueDB sqliteFile

runDialogueS' :: T.Text -> Dialogue a -> Script a
runDialogueS' = runDialogueS

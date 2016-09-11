{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}


module Dialog.Types where


import           Control.Error
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad.Base
import qualified Control.Monad.Catch         as Catch
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Control
import           Data.Data
import           GHC.Generics                hiding (to)


-- * Application Monad

data DialogueData = DialogueData
data DialogueState = DialogueState

newtype DialogueT e m a
    = DialogueT { unSay :: ExceptT e
                           (RWST DialogueData () DialogueState
                           (LoggingT m))
                           a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadLogger
             , MonadReader DialogueData, MonadState DialogueState
             , Generic
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

runDialogue :: Exception e => Dialogue e a -> IO a
runDialogue d =
    either (fail . displayException) return . fst
        =<< runStderrLoggingT (evalRWST (runExceptT (unSay d))
                                        DialogueData DialogueState)

runDialogueT :: MonadIO m => DialogueT e m a -> m (Either e a)
runDialogueT d =   runStderrLoggingT
               $   fst
               <$> evalRWST (runExceptT (unSay d)) DialogueData DialogueState

runDialogueL :: (Exception e, Monad m)
             => DialogueT e m a -> LoggingT (ExceptT e m) a
runDialogueL d = LoggingT
               $ ExceptT
               . fmap fst
               . runLoggingT
               ( evalRWST (runExceptT (unSay d)) DialogueData DialogueState)

-- * Services

data Service = Twitter
             -- | Google
             -- | IRC
             -- | DoNote
             -- | Journal
             deriving (Eq, Read, Show, Data, Typeable, Generic)

data ExportFormat = ExportJSON | ExportEPUB
                  deriving (Eq, Show, Data, Typeable, Generic)
